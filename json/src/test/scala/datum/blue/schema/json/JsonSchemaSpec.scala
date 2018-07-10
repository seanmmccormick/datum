package datum.blue.schema.json

import datum.blue.{meta, schema}
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
        "age" -> schemaFix.value(IntegerType, Map(meta.common.optional -> meta.property(true)))
      )(Map(meta.AttrKey("foo") -> meta.property("bar")))


      println(person.asJson.spaces2)
    }
  }

}
