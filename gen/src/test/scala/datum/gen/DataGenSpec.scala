package datum.gen
import datum.algebras.Corresponds
import datum.gen.algebras.DataGen
import datum.patterns.attributes._
import datum.modifiers.Optional
import datum.patterns.data.Data
import datum.patterns.schemas
import datum.patterns.schemas.{IntType, Schema, TextType}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.WordSpec
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers


class DataGenSpec extends WordSpec with Checkers {

  val test: Schema = schemas.obj()(
    "foo" -> schemas.value(IntType, Optional.key -> true),
    "bar" -> schemas.value(TextType)
  )

  val other: Schema = schemas.obj()(
    "no" -> schemas.value(IntType)
  )

  val generator = DataGen.using(DataGen.optional(DataGen.algebra))

  val testGen = generator(test)

  val genDataFromSchema: Gen[List[Data]] = {
    for {
      ds <- Gen.listOf(testGen)
    } yield ds
  }

  implicit val arb: Arbitrary[List[Data]] = Arbitrary(genDataFromSchema)

  val corresponds = new Corresponds(Corresponds.optional)

  val correspondsTest: Data => Boolean = corresponds.makeFn(test)

  val correspondsOther: Data => Boolean = corresponds.makeFn(other)

  "Generated Data" should {
    "correspond to a particular schema" in {
      check {
        forAll { data: List[Data] =>
          data.forall(correspondsTest) && !data.exists(correspondsOther)
        }
      }
    }
  }
}
