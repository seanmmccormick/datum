package datum.avrolib.data
import datum.avrolib.util.{Roundtrip, TestSchemas}
import datum.gen.algebras.{DataGen, SchemaGen}
import datum.patterns.data.Data
import datum.patterns.schemas
import datum.patterns.schemas.{BooleanType, IntType, Schema, TextType}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalatest.{Assertion, Matchers, WordSpec}
import org.scalatestplus.scalacheck.Checkers

class RecordReaderSpec extends WordSpec with Checkers with Matchers {

  def assertRoundtrip(schema: Schema): Assertion = {
    implicit val arb: Arbitrary[List[Data]] = Arbitrary {
      for {
        data <- Gen.nonEmptyListOf(DataGen.define()(schema))
      } yield data
    }

    check {
      forAll { data: List[Data] =>
        Roundtrip(schema, data) == data
      }
    }
  }

  "Avrolib RecordReader" should {

    "encode data for an indexed union" in {
      val schema = schemas.obj()(
        "test" -> schemas.indexed()(schemas.value(IntType), schemas.value(IntType)),
        "other" -> schemas.union()("a" -> schemas.value(BooleanType), "" -> schemas.value(TextType))
      )
      assertRoundtrip(schema)
    }

    "encode repeated unions" in {
      val schema = schemas.obj()(
        "union1" -> schemas.union()("a" -> schemas.value(BooleanType), "" -> schemas.value(TextType)),
        "union2" -> schemas.union()("a" -> schemas.value(IntType), "" -> schemas.value(IntType))
      )
      assertRoundtrip(schema)
    }

    "encode arrays" in {
      val schema = schemas.row()(
        schemas.col("array1", schemas.array()(schemas.value(IntType))),
        schemas.col("array2", schemas.array()(schemas.value(BooleanType)))
      )
      assertRoundtrip(schema)
    }

    "roundtrip all datatypes" in {
      assertRoundtrip(TestSchemas.types)
    }

    "roundtrip arbitrary schemas and data" in {
      val generator = SchemaGen.default
      val initial = Gen.oneOf(SchemaGen.AnObj, SchemaGen.ARow).map(SchemaGen.Seed(_, 5))
      implicit val arb: Arbitrary[(Schema, List[Data])] = Arbitrary {
        for {
          seed <- initial
          schema <- SchemaGen.define(SchemaGen.optional(generator.coalgebra))(seed)
          data <- Gen.nonEmptyListOf(DataGen.define(DataGen.optional(DataGen.algebra))(schema))
        } yield (schema, data)
      }

      check {
        forAll { generated: (Schema, List[Data]) =>
          val (schema, records) = generated
          Roundtrip(schema, records) == records
        }
      }
    }
  }
}
