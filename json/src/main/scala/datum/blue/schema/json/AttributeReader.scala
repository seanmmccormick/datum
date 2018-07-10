package datum.blue.schema.json

import datum.blue.attributes._
import io.circe.Decoder._
import io.circe.{Decoder, DecodingFailure, HCursor, Json}
import cats.syntax.either._
import cats.instances.either._
import turtles.{CoalgebraM, Corecursive}

object AttributeReader {

  def coalgebra[R](implicit R: Corecursive.Aux[R, AttrF]): CoalgebraM[Result, AttrF, HCursor] = {
    case cur if cur.value.isObject =>
      lazy val label = for {
        label <- cur.downField("label").success
        if label.value.isString
        value <- cur.downField("value").success
      } yield LabelF(label.value.asString.get, value)

      lazy val and = for {
        op <- cur.downField("op").success
        if op.value == Json.fromString("and")
        lhs <- cur.downField("lhs").success
        rhs <- cur.downField("rhs").success
      } yield AndF(lhs, rhs)

      lazy val or = for {
        op <- cur.downField("op").success
        if op.value == Json.fromString("or")
        lhs <- cur.downField("lhs").success
        rhs <- cur.downField("rhs").success
      } yield OrF(lhs, rhs)

      val result = label orElse and orElse or

      result.fold[Result[AttrF[HCursor]]](
        Left(DecodingFailure("Invalid Schema Object Attribute", cur.history))
      )(Right.apply)

    case cur if cur.value.isString => cur.as[String].map(TextPropertyF)

    case cur if cur.value.isNumber => cur.as[Double].map(NumericPropertyF)

    case cur if cur.value.isBoolean => cur.as[Boolean].map(BooleanPropertyF)

    case err => Left(DecodingFailure.apply("Invalid Schema Attribute", err.history))
  }

  implicit def decoder[R](implicit R: Corecursive.Aux[R, AttrF]): Decoder[R] = new Decoder[R] {
    override def apply(c: HCursor): Result[R] = R.anaM(c)(coalgebra)
  }
}
