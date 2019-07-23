package datum.avrolib.schemas
import datum.gen.algebras.SchemaGen
import datum.modifiers.Optional
import datum.patterns.schemas
import datum.patterns.schemas._
import datum.patterns.properties._

import datum.gen.algebras.SchemaGen.Seed
import higherkindness.droste.{Algebra, scheme}
import org.apache.avro.{Schema => AvroSchema}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.Checkers

class SchemaReadWriterSpec extends WordSpec with Checkers with Matchers {

  private val aSeed = Gen.oneOf(SchemaGen.AnObj, SchemaGen.ARow).map(Seed(_, 5))

  private val nonEmptyNames: Schema => Boolean = scheme.cata {
    Algebra[SchemaF, Boolean] {
      case ValueF(_, _) => true
      case ObjF(fields, _) =>
        fields.forall {
          case (k, v) => k != "" && v
        }
      case RowF(elems, _) => elems.forall(_.value == true)
      case NamedUnionF(alts, _) =>
        alts.forall {
          case (k, v) => k != "" && v
        }
      case IndexedUnionF(alts, _) => alts.forall(_ == true)
    }
  }

  "Avrolib" should {
    "be able to encode a simple schema" in {
      val simple = schemas.obj()(
        "foo" -> schemas.value(IntType),
        "bar" -> schemas.value(BooleanType, Optional.enable)
      )

      val result = SchemaReadWriter.toAvroSchema(simple)
      assert(result.getField("foo").schema().getType == AvroSchema.Type.INT)
      assert(result.getField("bar").schema().getType == AvroSchema.Type.BOOLEAN)
    }

    "work with unnamed columns" in {
      val csv = schemas.row("test" -> "works".prop)(
        schemas.Column(schemas.value(IntType)),
        schemas.Column(schemas.value(BooleanType)),
        schemas.Column(schemas.value(BooleanType), Some("Unnamed")),
        schemas.Column(schemas.value(BooleanType), Some("Unnamed1")),
        schemas.Column(schemas.value(TextType), Some("Unnamed1"))
      )
      val avro = SchemaReadWriter.toAvroSchema(csv)
      val schema = SchemaReadWriter.fromAvroSchema(avro)
      schema shouldBe csv
    }

    "work with repeated names" in {
      val csv = schemas.row("test" -> "works".prop)(
        schemas.col("foo", schemas.value(IntType)),
        schemas.col("foo", schemas.value(IntType)),
        schemas.col("foo", schemas.value(IntType))
      )
      val avro = SchemaReadWriter.toAvroSchema(csv)
      val schema = SchemaReadWriter.fromAvroSchema(avro)
      schema shouldBe csv
    }

    "be able to encode/decode an arbitrary schema to json" in {
      implicit val arb: Arbitrary[Schema] = Arbitrary {
        //val fn = SchemaGen.define(SchemaGen.optional(SchemaGen.default.coalgebra))
        val fn = SchemaGen.define(SchemaGen.simple.coalgebra)
        aSeed.flatMap(fn)
      }

      check {
        forAll { schema: Schema =>
          val avro = SchemaReadWriter.toAvroSchema(schema)
          val recovered = SchemaReadWriter.fromAvroSchema(avro)
          recovered == schema
        }
      }
    }
  }
}
