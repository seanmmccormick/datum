package datum.blue.schema.json

import datum.blue.{attributes, schema}
import datum.blue.schema._
import datum.blue.ops
import datum.blue.schema.json.Writer._
import datum.blue.schema.json.Reader._
import org.scalatest.{Matchers, WordSpec}
import turtles.data.Fix
import io.circe.syntax._
import io.circe.parser._
class JsonSchemaSpec extends WordSpec with Matchers {

  val schemaFix: schema.Specialize[Fix[SchemaF]] =
    schema.Specialize[Fix[SchemaF]]

  val person: Fix[SchemaF] = schemaFix.struct(
    "name" -> schemaFix.value(TextType),
    "age" -> schemaFix.value(IntegerType, Map(attributes.common.optional -> attributes.property(true))),
    "details" -> schemaFix.struct("accountId" -> schemaFix.value(IntegerType))(Map.empty)
  )(Map(attributes.AttrKey("foo") -> attributes.property("bar")))

  val timeseries: Fix[SchemaF] = schemaFix.row(
    Column(schemaFix.value(TextType)),
    Column(schemaFix.value(RealType)),
    Column(schemaFix.value(TextType))
  )(Map.empty)

  "schemas" should {
    "be writeable to json" in {
      println(person.asJson.spaces2)
      println(timeseries.asJson.noSpaces)
    }

    "be readable from json" in {
      person.asJson.as[Fix[SchemaF]] shouldEqual Right(person)
    }

    "read row oriented data from json" in {
      timeseries.asJson.as[Fix[SchemaF]] shouldEqual Right(timeseries)
    }

    "fail to read invalid schemas" in {
      val bad1 = parse("""{"invalid":[{"type":"text"},{"type":"real"},{"type":"text"}]}""").right.get
      bad1.as[Fix[SchemaF]] shouldBe a [Left[_, _]]

      val bad2 = parse("""{"row":[{"type":"text"},{"type":"unreal"},{"type":"text"}]}""").right.get
      bad2.as[Fix[SchemaF]] shouldBe a [Left[_, _]]
    }
  }

}
