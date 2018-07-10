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

  implicit val zz: KeyDecoder[AttrKey] = KeyDecoder.instance(x => Some(AttrKey(x)))

  implicit val readAttr: Decoder[Attr] = AttributeReader.decoder[Attr]

  implicit val readType: Decoder[Type] =
    Decoder.decodeString.emap(t => Type.fromString(t).toRight(s"Invalid Schema Value Type! (${t.take(10)})"))

  def coalgebra[R](implicit R: Corecursive.Aux[R, SchemaF]): CoalgebraM[Result, SchemaF, HCursor] = {
    case cur if cur.value.isObject && cur.value.asObject.exists(_.contains("type")) =>
      for {
        typ <- cur.downField("type").as[Type]
        attrs <- cur.downField("attributes").as[Map[AttrKey, Attr]]
      } yield ValueF(typ, attrs)

    case cur if cur.value.isObject && cur.value.asObject.exists(_.contains("struct")) =>
      for {
        elements <- cur.downField("struct").as[SortedMap[String, HCursor]]
        attrs <- cur.downField("attributes").as[Map[AttrKey, Attr]]
      } yield StructF(elements, attrs)
  }

  implicit def decoder[R](implicit R: Corecursive.Aux[R, SchemaF]): Decoder[R] = new Decoder[R] {
    override def apply(c: HCursor): Result[R] = R.anaM(c)(coalgebra)
  }
}
