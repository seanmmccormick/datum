package datum.green

import datum.green.patterns.schemas
import datum.green.patterns.schemas._
import datum.red.helpers.Coalgebra
import org.scalacheck.Gen
import qq.droste.CoalgebraM
import qq.droste.data.Fix

object SchemaGen {

  val types: List[Type] = List(
    BooleanType,
    IntegerType,
    TextType,
    DateType
  )

  val genValue: Gen[Schema] = Gen.oneOf(types).map { x => schemas.value(x) }

  def genColumn(level: Int): Gen[Column[Schema]] = for {
    header <- Gen.resize(3, Gen.alphaLowerStr)
    w = Math.max(4 - level, 0)
    value <- Gen.frequency(5 -> genValue, w -> genStruct(level +1), w -> genRow(level +1))
  } yield Column(value, Some(header))

  def genStructFields(level: Int): Gen[(String, Schema)] = for {
    k <- Gen.resize(5, Gen.alphaLowerStr)
    w = Math.max(4 - level, 0)
    v <- Gen.frequency(5 -> genValue, w -> genStruct(level + 1), w -> genRow(level + 1))
  } yield (k, v)

  def genStruct(level: Int = 0): Gen[Schema] = for {
    n <- Gen.chooseNum(1, 5)
    fields <- Gen.listOfN(n, genStructFields(level))
  } yield {
    schemas.struct()(fields:_*)
  }

  def genRow(level: Int = 0): Gen[Schema] = for {
    n <- Gen.chooseNum(1,5)
    columns <- Gen.containerOfN[Vector, Column[Schema]](n, genColumn(level))
  } yield schemas.row()(columns:_*)
}
