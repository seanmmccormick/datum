package datum.blue

import datum.blue.data.DataF
import datum.blue.ops.Corresponds
import datum.blue.schema.{IntegerType, SchemaF, TextType}
import org.scalatest.{Matchers, WordSpec}
import turtles.data.Fix

class ListSpec extends WordSpec with Matchers {

  "list schemas" should {
    "be able to check correspondence of a simple list schema" in {

      val fixS = schema.Specialize[Fix[SchemaF]]

      val person = fixS.row(
        Vector(
          fixS.value(TextType),
          fixS.value(IntegerType)
        )
      )

      val fixD = data.Specialize[Fix[DataF]]

      val p = fixD.row(
        Vector(
          fixD.text("bob"),
          fixD.integer(42)
        )
      )

      pprint.pprintln(person)

      Corresponds(person)(p) shouldBe true
    }
  }
}
