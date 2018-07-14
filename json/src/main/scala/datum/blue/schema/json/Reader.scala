package datum.blue.schema.json

import datum.blue.attributes.{Attr, AttrKey}
import datum.blue.schema._
import datum.blue.schema.json.AttributeReader._
import io.circe.Decoder._
import io.circe._
import cats.syntax.either._
import cats.instances.either._

import turtles.{CoalgebraM, Corecursive}

import scala.collection.immutable.SortedMap

object Reader {

  implicit val readAttrKey: KeyDecoder[AttrKey] = KeyDecoder.instance(x => Some(AttrKey(x)))

  implicit val readAttr: Decoder[Attr] = AttributeReader.decoder[Attr]

  implicit val readType: Decoder[Type] =
    Decoder.decodeString.emap(t => Type.fromString(t).toRight(s"Invalid Schema Value Type! (${t.take(10)})"))

  implicit val readColumn: Decoder[Column[HCursor]] =
    Decoder[Map[String, HCursor]].emap { col =>
      col.get("value").map { v =>
        val header = col.get("header").flatMap(_.value.asString)
        Right(Column(v, header))
      }.getOrElse(Left("Invalid Column Schema: Missing 'value'!"))
    }

  def coalgebra[R](implicit R: Corecursive.Aux[R, SchemaF]): CoalgebraM[Result, SchemaF, HCursor] = {
    case cur if cur.value.isObject && cur.value.asObject.exists(_.contains("type")) =>
      for {
        typ <- cur.downField("type").as[Type]
        attrs <- cur.downField("attributes").as[Option[Map[AttrKey, Attr]]]
      } yield {
        ValueF(typ, attrs.getOrElse(Map.empty))
      }

    case cur if cur.value.isObject && cur.value.asObject.exists(_.contains("struct")) =>
      for {
        elements <- cur.downField("struct").as[SortedMap[String, HCursor]]
        attrs <- cur.downField("attributes").as[Option[Map[AttrKey, Attr]]]
      } yield StructF(elements, attrs.getOrElse(Map.empty))

    case cur if cur.value.isObject && cur.value.asObject.exists(_.contains("row")) =>
      for {
        elements <- cur.downField("row").as[Vector[Column[HCursor]]]
        attrs <- cur.downField("attributes").as[Option[Map[AttrKey, Attr]]]
      } yield RowF(elements, attrs.getOrElse(Map.empty))

    case err => Left(DecodingFailure.apply("Invalid Schema Type", err.history))
  }

  implicit def decoder[R](implicit R: Corecursive.Aux[R, SchemaF]): Decoder[R] = new Decoder[R] {
    override def apply(c: HCursor): Result[R] = R.anaM(c)(coalgebra)
  }
}
