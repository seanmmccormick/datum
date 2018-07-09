package datum.blue

import datum.blue.attributes.{Flag, Optional}
import datum.blue.data.DataF
import datum.blue.ops.Corresponds
import datum.blue.schema._
import org.scalatest.{Matchers, WordSpec}
import turtles.data.Fix
import turtles.implicits._

class StructSpec extends WordSpec with Matchers {

  val schemaFix: schema.Specialize[Fix[SchemaF]] =
    schema.Specialize[Fix[SchemaF]]

  val dataFix: data.Specialize[Fix[DataF]] = data.Specialize[Fix[DataF]]

  "struct schemas" should {
    "be able to check correspondence of a simple struct schema" in {

      val person = schemaFix.struct(
        "name" -> schemaFix.value(TextType),
        "age" -> schemaFix.value(IntegerType)
      )(Map.empty)

      val good = dataFix.struct(
        "name" -> dataFix.text("bob"),
        "age" -> dataFix.integer(42)
      )

      Corresponds(person)(good) shouldBe true

      val bad = dataFix.struct(
        "nope" -> dataFix.text("bob"),
        "age" -> dataFix.integer(42)
      )

      Corresponds(person)(bad) shouldBe false
    }

    "be able to handle optional values" in {
      val person = schemaFix.struct(
        "name" -> schemaFix.value(TextType),
        "age" -> schemaFix.value(IntegerType, Map(Optional -> Flag(true)))
      )(Map.empty)

      val p = dataFix.struct("name" -> dataFix.text("wat"))

      Corresponds(person)(p) shouldBe true
    }
  }
}
