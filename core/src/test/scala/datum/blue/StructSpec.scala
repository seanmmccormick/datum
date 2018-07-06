package datum.blue

import datum.blue.attributes.{Flag, Optional}
import datum.blue.data.DataF
import datum.blue.ops.Corresponds
import datum.blue.schema._
import datum.blue.schema
import org.scalatest.{Matchers, WordSpec}
import turtles.data.Fix
import turtles.implicits._

import scala.collection.immutable.SortedMap

class StructSpec extends WordSpec with Matchers {

  val fixS: schema.Specialize[Fix[SchemaF]] = schema.Specialize[Fix[SchemaF]]

  val fixD: data.Specialize[Fix[DataF]] = data.Specialize[Fix[DataF]]

  "struct schemas" should {
    "be able to check correspondence of a simple struct schema" in {

      val person = fixS.struct(
        SortedMap(
          "name" -> fixS.value(TextType),
          "age" -> fixS.value(IntegerType)
        )
      )

      val good = fixD.struct(
        SortedMap(
          "name" -> fixD.text("bob"),
          "age" -> fixD.integer(42)
        )
      )

      Corresponds(person)(good) shouldBe true

      val bad = fixD.struct(
        SortedMap(
          "nope" -> fixD.text("bob"),
          "age" -> fixD.integer(42)
        )
      )

      Corresponds(person)(bad) shouldBe false
    }

    "be able to handle optional values" in {
      val person = fixS.struct(
        SortedMap(
          "name" -> fixS.value(TextType),
          "age" -> fixS.value(IntegerType, Map(Optional -> Flag(true)))
        )
      )

      val p = fixD.struct(SortedMap("name" -> fixD.text("wat")))

      Corresponds(person)(p) shouldBe true
    }
  }
}
