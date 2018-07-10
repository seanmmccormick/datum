package datum.blue.schema.json

import datum.blue.{attributes, schema}
import datum.blue.schema.{IntegerType, SchemaF}
import datum.blue.schema.json.Writer._

import org.scalatest.{Matchers, WordSpec}
import turtles.data.Fix
import io.circe.syntax._

class JsonSchemaSpec extends WordSpec with Matchers {

  val schemaFix: schema.Specialize[Fix[SchemaF]] =
    schema.Specialize[Fix[SchemaF]]



  "schemas" should {
    "be writeable to json" in {
      val person: Fix[SchemaF] = schemaFix.struct(
        "age" -> schemaFix.value(IntegerType, Map(attributes.common.optional -> attributes.property(true)))
      )(Map(attributes.AttrKey("foo") -> attributes.property("bar")))


      println(person.asJson.spaces2)
    }
  }

}
