package datum.blue

import datum.blue.schema.{IntegerType, SchemaF, StructF, TextType}
import datum.blue.transform.TransformF
import org.scalatest.{Matchers, WordSpec}
import turtles.{Algebra, Birecursive}
import turtles.data.Fix

class TransformSpec extends WordSpec with Matchers {

  val schemaFix: schema.Specialize[Fix[SchemaF]] =
    schema.Specialize[Fix[SchemaF]]

  val transformFix: transform.Specialize[Fix[TransformF]] = {
    transform.Specialize[Fix[TransformF]]
  }

  val person = schemaFix.struct(
    "name" -> schemaFix.value(TextType),
    "age" -> schemaFix.value(IntegerType),
    "food" -> schemaFix.value(TextType)
  )(Map.empty)

  type Sch = Fix[SchemaF]

  val wat: Algebra[TransformF, Sch => Option[Sch]] = {
    case transform.StructF(fields) => {
      case Fix(schema.StructF(schFields, attrs)) =>
        val modified = fields.map {
          case (k, v) =>
            (k, v(schFields(k)).get)
        }
        Option(Fix(StructF(modified)))

      case _ => None
    }

    case transform.KeepF =>
      x =>
        println("KKeeping!")
        pprint.pprintln(x)
        Option(x)
    case _ =>
      _ =>
        None
  }

  "transforms" should {
    "zzz" in {
      val hrm = transformFix.struct(
        "name" -> transformFix.keep,
        "food" -> transformFix.keep
      )
      val ok = Birecursive[Fix[TransformF]].cata(hrm)(wat)
      pprint.pprintln(ok(person))
    }
  }
}
