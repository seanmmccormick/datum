package datum.blue.schema.json

import datum.blue.attributes
import datum.blue.attributes._
import datum.blue.schema._
import io.circe
import io.circe.{Encoder, Json}
import io.circe._
import io.circe.syntax._
import turtles.{Algebra, Corecursive, Recursive}

class Writer[Extended <: AttributeValue](extended: PartialFunction[Extended, Json] = PartialFunction.empty) {

  implicit val foo: KeyEncoder[AttributeKey] = KeyEncoder.instance {
    case attributes.Optional          => "optional"
    case attributes.TextRefinement    => "text_refinement"
    case attributes.NumericRefinement => "numeric_refinement"
    case attributes.Key(value)        => value
  }

  private def textRefinementJs(inp: TextRefinementAttribute): Json = inp match {
    case attributes.RegexRefinement(expr) => Json.obj("regex" -> Json.fromString(expr))
  }

  private def numericRefinementJs(inp: NumericRefinementAttribute): Json = inp match {
    case attributes.Positive           => Json.fromString("positive")
    case attributes.LessThan(max)      => Json.obj("less_than" -> Json.fromDoubleOrNull(max))
    case attributes.GreaterThan(min)   => Json.obj("greater_than" -> Json.fromDoubleOrNull(min))
    case attributes.UnitOfMeasure(uom) => Json.obj("uom" -> Json.fromString(uom))
  }

  private val common: PartialFunction[AttributeValue, Json] = {
    case attributes.Flag(x)                       => Json.fromBoolean(x)
    case attributes.Property(v)                   => Json.fromString(v)
    case a: attributes.TextRefinementAttribute    => textRefinementJs(a)
    case a: attributes.NumericRefinementAttribute => numericRefinementJs(a)
  }

  implicit val bar: Encoder[AttributeValue] = new Encoder[AttributeValue] {
    override def apply(a: AttributeValue): Json = (common ).apply(a)
  }

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
