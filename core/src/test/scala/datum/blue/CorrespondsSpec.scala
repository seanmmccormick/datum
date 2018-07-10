package datum.blue

import datum.blue.data.{DataF, TextDataF}
import datum.blue.attributes.Attributes
import datum.blue.ops.Corresponds
import datum.blue.schema.{IntegerType, SchemaF, TextType}
import org.scalatest.{Matchers, WordSpec}
import turtles.data.Fix

class CorrespondsSpec extends WordSpec with Matchers {

  val schemaFix: schema.Specialize[Fix[SchemaF]] =
    schema.Specialize[Fix[SchemaF]]

  val dataFix: data.Specialize[Fix[DataF]] = data.Specialize[Fix[DataF]]

  "correspondence checking" should {
    "be extensible" in {
      val person: Fix[SchemaF] = schemaFix.struct(
        "age" -> schemaFix.value(IntegerType)
      )(Map.empty)

      val good = dataFix.struct(
        "age" -> dataFix.integer(42)
      )

      Corresponds.partial(person, good) {
        case (data.IntegerDataF(v), _) if v >= 200 => false
      } shouldBe true

      val bad = dataFix.struct(
        "age" -> dataFix.integer(500)
      )

      Corresponds.partial(person, bad) {
        case (data.IntegerDataF(v), _) if v >= 200 => false
      } shouldBe false
    }

    "be able to compose the default checks with custom checks" in {

      def onlyBob[Data]: PartialFunction[(DataF[Data], Attributes), Boolean] = {
        case (TextDataF("bob"), _) => true
        case (TextDataF(_), _)     => false
      }

      val sch: Fix[SchemaF] = schemaFix.struct(
        "name" -> schemaFix.value(TextType, Map(attributes.common.optional -> attributes.property(true)))
      )(Map.empty)

      val bob = dataFix.struct("name" -> dataFix.text("bob"))

      val sam = dataFix.struct("name" -> dataFix.text("sam"))

      val maybeSam = dataFix.struct("name" -> dataFix.empty)

      val composedChecks = onlyBob[Fix[DataF]] orElse Corresponds.checks.default

      Corresponds.partial(sch, bob) { composedChecks } shouldBe true

      Corresponds.partial(sch, sam) { composedChecks } shouldBe false

      Corresponds.partial(sch, maybeSam) { composedChecks } shouldBe true

    }
  }
}
