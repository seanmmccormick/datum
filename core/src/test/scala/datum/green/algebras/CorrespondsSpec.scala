package datum.green.algebras
import datum.green.{DataGen, SchemaGen}
import datum.green.patterns.schemas.{IntType, Schema, TextType}
import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import datum.green.patterns.data.Data
import datum.green.patterns.schemas
import org.scalacheck.{Arbitrary, Gen}

class CorrespondsSpec extends WordSpec with Checkers {

  val genDataFromSchema: Gen[(Schema, List[Data])] = {
    for {
      s <- SchemaGen.genStruct()
      d <- DataGen.generatorFor(s)
      ds <- Gen.listOf(d)
    } yield (s, ds)
  }

  implicit val arb: Arbitrary[(Schema, List[Data])] = Arbitrary(genDataFromSchema)

  val correspondsFn = new Corresponds(identity)

  val otherSchema: Schema = schemas.struct()(
    "foo" -> schemas.value(IntType),
    "bar" -> schemas.value(TextType)
  )

  val negFn: Data => Boolean = correspondsFn.generateFor(otherSchema)

  "The correspondence function" should {
    "work for schema generated data" in {
      check {
        forAll { generated: (Schema, List[Data]) =>
          val (schema, data) = generated
          val fn: Data => Boolean = correspondsFn.generateFor(schema)
          data.forall(fn) && (data.isEmpty || !data.forall(negFn))
        }
      }
    }
  }
}
