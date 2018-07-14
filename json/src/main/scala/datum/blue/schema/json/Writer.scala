package datum.blue.schema.json

import datum.blue.attributes.AttrKey
import datum.blue.schema._
import datum.blue.schema.json.AttributeWriter._
import io.circe.{Encoder, Json, KeyEncoder}
import io.circe.syntax._
import turtles.{Algebra, Recursive}

object Writer {

  implicit val keyEncAttr: KeyEncoder[AttrKey] = KeyEncoder.instance(_.value)

  implicit val columnEnc: Encoder[Column[Json]] = Encoder[Map[String, Json]].contramap { col =>
    Map("header" -> col.header.asJson, "value" -> col.value)
  }

  val algebra: Algebra[SchemaF, Json] = {
    case ValueF(typ, attrs) if attrs.nonEmpty =>
      Json.obj("type" -> Json.fromString(Type.asString(typ)), "attributes" -> attrs.asJson)
    case ValueF(typ, _) => Json.obj("type" -> Json.fromString(Type.asString(typ)))

    case RowF(elements, attrs) if attrs.nonEmpty => Json.obj("row" -> elements.asJson, "attributes" -> attrs.asJson)
    case RowF(elements, _)                       => Json.obj("row" -> elements.asJson)

    case StructF(fields, attrs) if attrs.nonEmpty => Json.obj("struct" -> fields.asJson, "attributes" -> attrs.asJson)
    case StructF(fields, _)                       => Json.obj("struct" -> fields.asJson)

  }

  implicit def encode[R](implicit R: Recursive.Aux[R, SchemaF]): Encoder[R] = new Encoder[R] {
    override def apply(a: R): Json = R.cata(a)(algebra)
  }
}
