package datum.ujsonlib
import datum.gen.algebras.{DataGen, SchemaGen}
import datum.gen.algebras.SchemaGen._
import datum.patterns.{data => d}
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

  private val aSeed = Gen.oneOf(SchemaGen.AUnion, SchemaGen.AUnion).map(Seed(_, 5))

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
          pprint.pprintln(js.render(2))
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

    "union should work with rows" in {
      val schema = s.union()(
        s.row()(Column(s.value(BooleanType))),
        s.value(DateType),
        s.array()(s.value(LongType))
      )
      val toJs = WriteJs.define()(schema)
      val fromJs = ReadJs.define()(schema)
      val check = datum.patterns.data.row(datum.patterns.data.long(1),datum.patterns.data.long(2))

      pprint.pprintln(fromJs(toJs(check)))
      fromJs(toJs(check)) shouldBe Right(check)
    }

    "union should work with objs" in {
      val schema = s.union()(
        s.obj()("a" -> s.value(BooleanType)),
        s.obj()("b" -> s.value(BooleanType)),
      )
      val toJs = WriteJs.define()(schema)
      val fromJs = ReadJs.define()(schema)
      val check = d.obj("b" -> d.boolean(true))

      fromJs(toJs(check)) shouldBe Right(check)
    }

    "union should work with empty array" in {
      val schema = s.union()(
        s.value(BooleanType),
        s.array()(s.value(BooleanType)),
      )
      val toJs = WriteJs.define()(schema)
      val fromJs = ReadJs.define()(schema)
      val check = d.row()

      fromJs(toJs(check)) shouldBe Right(check)
    }

    "union should work..maybe?" in {
      val schema = s.union()(
        s.array()(s.value(TextType)),
        s.array()(s.value(BooleanType)),
      )
      val toJs = WriteJs.define()(schema)
      val fromJs = ReadJs.define()(schema)
      val check = d.row(d.boolean(true))

      fromJs(toJs(check)) shouldBe Right(check)
    }

    "what should we do about this" in {
      val schema = s.union()(
        //s.array()(s.value(LongType)),
        s.value(BooleanType),
        s.array()(s.union()(s.value(BooleanType), s.value(LongType))),
      )

      val toJs = WriteJs.define()(schema)
      val fromJs = ReadJs.define()(schema)

      val check = d.row(d.boolean(true), d.long(42))

      fromJs(toJs(check)) shouldBe Right(check)
    }

    "be able to encode/decode arbitrary data of some schema" in {
//      val generator = new SchemaGen(
//        next = Gen.frequency(
//          4 -> Gen.const(AValue),
//          //1 -> Gen.const(AnObj),
//          //1 -> Gen.const(ATable),
//          //1 -> Gen.const(AUnion),
//          1 -> Gen.const(AnArray)
//        )
//      )

      val generator = SchemaGen.default

      implicit val arb: Arbitrary[(Schema, List[Data])] = Arbitrary {
        for {
          seed <- aSeed
          schema <- SchemaGen.define(generator.coalgebra)(seed)
          data <- Gen.nonEmptyListOf(DataGen.define()(schema))
        } yield (schema, data)
      }

      check {
        forAll { generated: (Schema, List[Data]) =>
          val (schema, data) = generated
          val toJs = WriteJs.define()(schema)
          val fromJs = ReadJs.define()(schema)
          //data.map(d => reader(writer(d))) == data.map(Right.apply)
          data.map(d => (fromJs(toJs(d)), d)).collect {
            case (Right(x), q) if x != q =>
              pprint.pprintln(s"Failed: ${x}")
              pprint.pprintln("==== DATA ====")
              pprint.pprintln(q)
              pprint.pprintln("===== GOT ====")
              pprint.pprintln(x)
              pprint.pprintln("==== SCHEMA ====")
              pprint.pprintln(schema)
              ()
          }

          //data.map(d => fromJs(toJs(d))).forall(_.isRight) && true

          data.map(d => fromJs(toJs(d))) == data.map(Right.apply)
        }
      }
    }
  }
}
