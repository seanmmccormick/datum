package datum.gen
import datum.algebras.Corresponds
import datum.gen.algebras.DataGen
import datum.patterns.attributes._
import datum.modifiers.Optional
import datum.patterns.data.Data
import datum.patterns.{data, schemas}
import datum.patterns.schemas.{IntType, Schema, TextType}
import org.scalacheck.Arbitrary
import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers


class DataGenSpec extends WordSpec with Checkers with Matchers {

  val test: Schema = schemas.obj()(
    "foo" -> schemas.value(IntType, Optional.key -> true),
    "bar" -> schemas.value(TextType)
  )

  val other: Schema = schemas.obj()(
    "no" -> schemas.value(IntType)
  )

  val generator = DataGen.using()

  val testGen = generator(test)

  val correspondsTo = Corresponds.using(Corresponds.optional(Corresponds.algebra))

  val correspondsTest: Data => Boolean = correspondsTo(test)

  val correspondsOther: Data => Boolean = correspondsTo(other)

  "Generated Data" should {
    "correspond to a particular schema" in {
      implicit val arb: Arbitrary[Data] = Arbitrary(testGen)
      check {
        forAll { data: Data =>
          correspondsTest(data) && !correspondsOther(data)
        }
      }
    }

    "generate optional values" in {
      val schema = schemas.array()(schemas.value(IntType, Optional.key -> true))
      val dataOf = DataGen.using(DataGen.optional(DataGen.algebra))
      val testFn = correspondsTo(schema)

      val sample = data.row(data.integer(1), data.empty, data.integer(2))
      testFn(sample) shouldBe true

      implicit val arb: Arbitrary[Data] = Arbitrary(dataOf(schema))

      check {
        forAll { data: Data =>
          testFn(data)
        }
      }
    }
  }
}
