package datum.blue.schema.json

import datum.blue.attributes._
import datum.blue.schema
import datum.blue.schema.{IntegerType, SchemaF}
import org.scalatest.{Matchers, WordSpec}
import turtles.data.Fix
import io.circe.syntax._

class JsonSchemaSpec extends WordSpec with Matchers {

  val schemaFix: schema.Specialize[Fix[SchemaF]] =
    schema.Specialize[Fix[SchemaF]]

  val wurt: Writer[AttributeValue] = new Writer()
  import wurt._

  "schemas" should {
    "be writeable to json" in {
      val person: Fix[SchemaF] = schemaFix.struct(
        "age" -> schemaFix.value(IntegerType, Map(Optional -> Flag(true)))
      )(Map(Key("foo") -> Property("bar")))


      println(person.asJson.spaces2)
    }
  }

}
