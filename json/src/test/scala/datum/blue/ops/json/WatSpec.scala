package datum.blue.ops.json

import datum.blue.{attributes, schema}
import datum.blue.schema.{BooleanType, SchemaF, TextType}
import io.circe._
import io.circe.parser._
import org.scalatest.{Matchers, WordSpec}
import turtles.data.Fix

class WatSpec extends WordSpec with Matchers {

  val schemaFix: schema.Specialize[Fix[SchemaF]] =
    schema.Specialize[Fix[SchemaF]]

  val sample: Fix[SchemaF] = schemaFix.struct(
    "name" -> schemaFix.value(TextType),
    "missing" -> schemaFix.value(TextType, Map(attributes.common.optional -> attributes.property(true))),
    "omg" -> schemaFix.value(BooleanType)
  )(Map(attributes.AttrKey("foo") -> attributes.property("bar")))

  "blah" should {
    "blah" in {
      val neat = parse("""{"omg":false, "name":"Bob"}""").flatMap { js =>
        schema.json.Parse2(sample, js)(schema.json.checks.optional)
      }
      pprint.pprintln(neat)
    }
  }
}
