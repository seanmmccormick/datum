package datum

import datum.patterns.schemas
import datum.patterns.schemas._

import org.scalacheck.Gen

object SchemaGen {

  val MAX_DEPTH = 4

  val types: List[Type] = List(
    IntType,
    LongType,
    InstantType,
    DoubleType,
    FloatType,
    TextType,
    BytesType,
    BooleanType,
    DateType,
    LocalTimeType,
    ZonedTimeType
  )

  val genValue: Gen[Schema] = Gen.oneOf(types).map { x =>
    schemas.value(x)
  }

  def genColumn(level: Int): Gen[Column[Schema]] =
    for {
      header <- Gen.resize(3, Gen.alphaLowerStr)
      w = Math.max(MAX_DEPTH - level, 0)
      value <- Gen.frequency(5 -> genValue, w -> genStruct(level + 1), w -> genRow(level + 1), w -> genUnion(level + 1))
    } yield Column(value, Some(header))

  def genStructFields(level: Int): Gen[(String, Schema)] =
    for {
      k <- Gen.resize(5, Gen.alphaLowerStr)
      w = Math.max(MAX_DEPTH - level, 0)
      v <- Gen.frequency(5 -> genValue, w -> genStruct(level + 1), w -> genRow(level + 1), w -> genUnion(level + 1))
    } yield (k, v)

  def genStruct(level: Int = 0): Gen[Schema] =
    for {
      n <- Gen.chooseNum(1, 5)
      fields <- Gen.listOfN(n, genStructFields(level))
    } yield {
      schemas.obj()(fields: _*)
    }

  def genUnion(level: Int = 0): Gen[Schema] =
    for {
      n <- Gen.chooseNum(2, 4)
      w = Math.max(MAX_DEPTH - level, 0)
      alts <- Gen.listOfN(n, Gen.frequency(5 -> genValue, w -> genStruct(level + 1), w -> genRow(level + 1)))
    } yield {
      schemas.union()(alts: _*)
    }

  def genRow(level: Int = 0): Gen[Schema] =
    for {
      n <- Gen.chooseNum(1, 5)
      columns <- Gen.containerOfN[Vector, Column[Schema]](n, genColumn(level))
    } yield schemas.row()(columns: _*)
}
