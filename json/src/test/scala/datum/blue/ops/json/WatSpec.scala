package datum.blue.ops.json

import datum.blue.attributes.Attributes
import datum.blue.data.{DataF, TextDataF}
import datum.blue.{attributes, schema}
import datum.blue.schema.{BooleanType, SchemaF, TextType}
import io.circe.Decoder.Result
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
      def notbob: PartialFunction[(Result[Fix[DataF]],Attributes), Result[Fix[DataF]]] = {
        case (Right(Fix(TextDataF(x))), _) if x == "bob" => Left(DecodingFailure("NO BOBS!", List.empty))
      }

      val neat = parse("""{"omg":false, "name":"bob"}""").flatMap { js =>
        schema.json.Parse2(sample)(schema.json.checks.optional)(js)
        //schema.json.Parse2(sample, js)(PartialFunction.empty)
      }
      pprint.pprintln(neat)
    }
  }
}
