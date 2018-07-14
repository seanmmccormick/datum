package datum.blue

import datum.blue.schema.{Column, IntegerType, SchemaF, TextType}
import datum.blue.transform.TransformF
import org.scalatest.{Matchers, WordSpec}
import turtles.data.Fix
import datum.blue.data.DataF
import datum.blue.ops.{TransformData, TransformSchema}


class TransformSpec extends WordSpec with Matchers {

  private val sch: schema.Specialize[Fix[SchemaF]] =
    schema.Specialize[Fix[SchemaF]]

  private val dat: data.Specialize[Fix[DataF]] = data.Specialize[Fix[DataF]]

  private val trn: transform.Specialize[Fix[TransformF]] = {
    transform.Specialize[Fix[TransformF]]
  }

  private val person = sch.struct(
    "name" -> sch.value(TextType),
    "age" -> sch.value(IntegerType),
    "food" -> sch.value(TextType)
  )(Map.empty)

  "transforms" should {
    "be able to keep a subset of fields" in {
      val test = trn.struct(
        "name" -> trn.keep,
        "food" -> trn.keep
      )
      val result = TransformSchema(test)(person)

      result shouldBe Some(
        sch.struct(
          "name" -> sch.value(TextType),
          "food" -> sch.value(TextType)
        )(Map.empty))
    }

    "be able to 'explode' a struct into a row" in {
      val explode = trn.explode(
        trn.struct(
          "name" -> trn.keep,
          "food" -> trn.keep
        )
      )

      TransformSchema(explode)(person) shouldBe Some(
        sch.row(Column(sch.value(TextType), Some("food")), Column(sch.value(TextType), Some("name")))()
      )
    }

    "be able to keep struct fields by default" in {
      val kept = trn.struct(
        "food" -> trn.drop
      )

      TransformSchema(kept, keepByDefault = true)(person) shouldBe Some(
        sch.struct("name" -> sch.value(TextType), "age" -> sch.value(IntegerType))(Map.empty)
      )
    }

    "be able to transform data" in {
      val bob = dat.struct("name" -> dat.text("bob"), "age" -> dat.integer(42), "food" -> dat.text("pizza"))
      val test = trn.struct(
        "name" -> trn.keep,
        "food" -> trn.keep
      )
      val ok = TransformData(test)(bob)
      pprint.pprintln(ok)
    }
  }
}
