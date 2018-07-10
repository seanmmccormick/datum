package datum.blue.schema.json

import datum.blue.attributes.AttrKey
import datum.blue.schema._
import datum.blue.schema.json.AttributeWriter._
import io.circe.{Encoder, Json, KeyEncoder}
import io.circe.syntax._
import turtles.{Algebra, Recursive}

object Writer {

  implicit val keyEncAttr: KeyEncoder[AttrKey] = KeyEncoder.instance(_.value)

  val algebra: Algebra[SchemaF, Json] = {
    case ValueF(typ, attrs) =>
      Json.obj("value" -> Json.fromString(Type.asString(typ)), "attributes" -> attrs.asJson)

    case RowF(elements, attrs) => Json.obj("row" -> elements.asJson, "attributes" -> attrs.asJson)

    case StructF(fields, attrs) => Json.obj("struct" -> fields.asJson, "attributes" -> attrs.asJson)

  }

  implicit def encode[R](implicit R: Recursive.Aux[R, SchemaF]): Encoder[R] = new Encoder[R] {
    override def apply(a: R): Json = R.cata(a)(algebra)
  }
}
