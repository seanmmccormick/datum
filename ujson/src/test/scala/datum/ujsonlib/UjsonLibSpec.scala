package datum.ujsonlib
import datum.gen.algebras.{DataGen, SchemaGen}
import datum.gen.algebras.SchemaGen.{AValue, Seed}
import datum.patterns.data.Data
import datum.patterns.schemas._
import datum.patterns.{schemas => s}
import datum.ujsonlib.data.{ReadJs, WriteJs}
import datum.ujsonlib.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers

class UjsonLibSpec extends WordSpec with Checkers with Matchers {

  private val aSeed = Gen.oneOf(SchemaGen.AnObj, SchemaGen.AnObj).map(Seed(_, 5))

  private val generateUsing = DataGen.define()
  implicit def arbData(implicit schema: Schema): Arbitrary[Data] = Arbitrary {
    generateUsing(schema)
  }

  "UJson Lib" should {
    "be able to encode/decode an arbitrary schema to json" in {
      implicit val arb: Arbitrary[Schema] = Arbitrary {
        val fn = SchemaGen.define(SchemaGen.optional(SchemaGen.default.coalgebra))
        aSeed.flatMap(fn)
      }

      check {
        forAll { schema: Schema =>
          val js = upickle.default.writeJs(schema)
          val parsed = upickle.default.readJs[Schema](js)
          parsed == schema
        }
      }
    }

    "roundtrip a simple obj" in {
      implicit val schema: Schema = s.obj()("a" -> s.value(IntType), "b" -> s.value(TextType))
      val toJs = WriteJs.define()(schema)
      val fromJs = ReadJs.define()(schema)
      check {
        forAll { data: Data =>
          fromJs(toJs(data)) == Right(data)
        }
      }
    }

    "be able to encode/decode arbitrary data of some schema" in {

      val test =     new SchemaGen(
        allowedValueTypes = Vector(IntType, BooleanType, TextType, TimestampType),
        next = Gen.const(AValue)
      )
      implicit val arb: Arbitrary[(Schema, List[Data])] = Arbitrary  {
        for {
          seed <- aSeed
          schema <- SchemaGen.define(test.coalgebra)(seed)
          data <- Gen.nonEmptyListOf(DataGen.define()(schema))
        } yield (schema, data)
      }

      check {
        forAll { generated: (Schema, List[Data]) =>
          val (schema, data) = generated
          //pprint.pprintln(data.head)
          val fn = WriteJs.define()(schema)
          val js = fn(data.head)
          println("#################")
          pprint.pprintln(schema)
          pprint.pprintln(js.render(2))
          true
        }
      }
    }
  }
}
