package datum.blue.ops.json

import datum.blue.attributes.{AttrKey, Attributes}
import datum.blue.data.{DataF, NullDataF, TextDataF}
import datum.blue.ops.DefaultRules
import datum.blue.ops.DefaultRules.InvalidDefaultAttributeValue
import datum.blue.schema.json.{Parse4, modifiers}
import datum.blue.{attributes, schema}
import datum.blue.schema.{BooleanType, IntegerType, SchemaF, TextType}
import io.circe.Decoder.Result
import io.circe._
import io.circe.parser._
import org.scalatest.{Matchers, WordSpec}
import turtles.data.Fix
import turtles.implicits._

class WatSpec extends WordSpec with Matchers {

  val fix: schema.Specialize[Fix[SchemaF]] =
    schema.Specialize[Fix[SchemaF]]

  val sample: Fix[SchemaF] = fix.struct(
    "name" -> fix.value(TextType),
    "missing" ->
      fix.value(
        BooleanType,
        Map(
          attributes.common.optional -> attributes.property(true),
          AttrKey("default") -> attributes.property(true)
        )
      ),
    "omg" -> fix.value(BooleanType, Map(attributes.common.optional -> attributes.property(false))),
    "age" -> fix.value(IntegerType, Map(attributes.common.default -> attributes.property(42)))
  )(Map(attributes.AttrKey("foo") -> attributes.property("bar")))

  "blah" should {
    "blah" in {
      def notbob: PartialFunction[(Result[Fix[DataF]], Attributes), Result[Fix[DataF]]] = {
        case (Right(Fix(TextDataF(x))), _) if x == "bob" => Left(DecodingFailure("NO BOBS!", List.empty))
      }

      val neat = parse("""{"omg":false, "name":"bob"}""").flatMap { js =>
        schema.json.Parse2(sample)(schema.json.checks.optional)(js)
      }
      pprint.pprintln(neat)
    }

    import cats.implicits._

//    "other" in {
//      val neat = parse("""{"omg":false, "name":"bob"}""").flatMap { js =>
//        //val qq = modifiers.optional[Fix] _ >>> modifiers.withDefaults[Fix]
////        val defaults = new DefaultsAttempt2(new DefaultRules)
////        val defaultCheck = sample.cata(
////          defaults.algebra[Result[Fix[DataF]], Fix](
////            _.leftMap {
////              case InvalidDefaultAttributeValue(msg) =>
////                DecodingFailure(msg, Nil)
////            },
////            {
////              case Left(_) =>
////                println("LLL?")
////                State.pure(true)
////              case Right(Fix(NullDataF)) =>
////                println("AAA?")
////                State.pure(true)
////              case _ =>
////                println("eh?")
////                State.pure(false)
////            }
//////            _.map {
//////              case Left(_)               => true
//////              case Right(Fix(NullDataF)) => true
//////              case _                     => false
//////            }
////          ))
////        //val neat = modifiers.optional[Fix] _ >>> defaultCheck
////        //schema.json.Parse3.wat[Fix](sample, defaultCheck)(js)
//        val parseBuilder = new Parse4(new DefaultRules)
//        val parseFn = parseBuilder(sample)
//        val result = parseFn(js)
//        result
//      }
//
//      pprint.pprintln(neat)
//    }

    "superneat" in {
      val neat = parse("""{"omg":false, "name":"bob", "age":null}""").flatMap { js =>
        val parseBuilder = new Parse4(new DefaultRules)
        val result: Either[InvalidDefaultAttributeValue, Json => Result[Fix[DataF]]] = parseBuilder(sample)
        result.leftMap(err => DecodingFailure(err.msg, Nil)).map { fn =>
          fn(js)
        }
      }
      println("============================")
      pprint.pprintln(neat)
    }
  }
}
