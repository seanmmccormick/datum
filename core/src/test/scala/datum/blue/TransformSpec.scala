package datum.blue

import datum.blue.schema.{IntegerType, SchemaF, TextType}
import datum.blue.transform.TransformF
import org.scalatest.{Matchers, WordSpec}
import turtles.{Algebra, Birecursive}
import turtles.data.Fix
import cats.instances.sortedMap._
import cats.instances.string._
import cats.instances.option._
import cats.instances.list._
import cats.syntax.traverse._
import datum.blue.data.DataF
import datum.blue.ops.{TransformData, TransformSchema}

import scala.collection.immutable.SortedMap

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
        sch.row(sch.value(TextType), sch.value(TextType))()
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
