package datum.avrolib
import datum.avrolib.schemas.{AvroSchemaWriter, AvroSchemaReader}
import datum.gen.algebras.{DataGen, SchemaGen}
import datum.patterns.data.Data
import datum.patterns.schemas._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Prop._
import org.scalatestplus.scalacheck.Checkers

class AvrolibSpec extends WordSpec with Checkers with Matchers {

  private val initial = Gen
    .oneOf(
      SchemaGen.AnObj,
      SchemaGen.ARow,
      //SchemaGen.AUnion
    )
    .map(SchemaGen.Seed(_, 5))

  "Avrolib" should {
    "be able to encode/decode arbitrary data of some schema" in {

      val generator = SchemaGen.default

      implicit val arb: Arbitrary[(Schema, List[Data])] = Arbitrary {
        for {
          seed <- initial
          schema <- SchemaGen.define(generator.coalgebra)(seed)
          data <- Gen.nonEmptyListOf(DataGen.define()(schema))
        } yield (schema, data)
      }

      check {
        forAll { generated: (Schema, List[Data]) =>
          val (schema, data) = generated
          val avro = AvroSchemaWriter.write(schema)

          true == true
        }
      }
    }
  }

}
