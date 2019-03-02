package datum.gen.algebras
import datum.modifiers.Optional
import datum.patterns.attributes._
import datum.patterns.schemas._
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import qq.droste.{CoalgebraM, scheme}

import scala.collection.immutable.SortedMap

class SchemaGen(
  allowedValueTypes: Vector[Type] = SchemaGen.types.all,
  allowedNestedTypes: Vector[(Int, SchemaGen.Next)] = SchemaGen.nest.default,
  objMaxNumFields: Int = 5,
  rowMaxNumColumns: Int = 5
) {
  import SchemaGen._

  private val genAllowedNested =
    Gen.frequency(allowedNestedTypes.map { case (freq, next) => (freq, Gen.const(next)) }: _*)

  private def nest(level: Int): Gen[Seed] = {
    for {
      n <- {
        if (level > 0) genAllowedNested
        else Gen.const(AValue)
      }
      l <- if (level > 1) Gen.chooseNum(0, level - 1) else Gen.const(0)
    } yield Seed(n, l)
  }

  private val genValue: Gen[SchemaF[Seed]] = Gen.oneOf(allowedValueTypes).map { x =>
    ValueF(x)
  }
  private def genObj(level: Int): Gen[SchemaF[Seed]] = {
    val genField = for {
      k <- Gen.resize(5, Gen.alphaLowerStr)
      s <- nest(level)
    } yield (k, s)

    Gen.resize(objMaxNumFields, Gen.listOf(genField).map { fields =>
      ObjF[Seed](SortedMap(fields: _*))
    })
  }

  private def genTable(level: Int): Gen[SchemaF[Seed]] = {
    val col: Gen[Column[Seed]] = for {
      k <- Gen.option(Gen.resize(3, Gen.asciiPrintableStr))
      s <- nest(level)
    } yield Column(s, k)

    Gen.resize(rowMaxNumColumns, Gen.nonEmptyContainerOf[Vector, Column[Seed]](col).map { elements =>
      RowF[Seed](elements)
    })
  }

  private def genUnion(level: Int): Gen[SchemaF[Seed]] = {
    Gen.resize(3, Gen.nonEmptyContainerOf[Vector, Seed](nest(level)).map { alts =>
      UnionF[Seed](alts)
    })
  }

  private def genArray(level: Int): Gen[SchemaF[Seed]] = {
    nest(level).map { s =>
      ArrayF[Seed](s)
    }
  }

  val coalgebra: CoalgebraM[Gen, SchemaF, Seed] = CoalgebraM {
    case Seed(AValue, _)      => genValue
    case Seed(AnObj, level)   => genObj(level)
    case Seed(ATable, level)  => genTable(level)
    case Seed(AUnion, level)  => genUnion(level)
    case Seed(AnArray, level) => genArray(level)
  }
}

object SchemaGen {

  object types {

    val all: Vector[Type] = Vector(
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
  }

  object nest {

    val default: Vector[(Int, Next)] = Vector(
      4 -> AValue,
      1 -> AnObj,
      1 -> ATable,
      1 -> AUnion,
      1 -> AnArray
    )
  }

  sealed trait Next
  case object AValue extends Next
  case object AnObj extends Next
  case object ATable extends Next
  case object AUnion extends Next
  case object AnArray extends Next

  case class Seed(next: Next, level: Int)

  val default = new SchemaGen()

  def optional(alg: CoalgebraM[Gen, SchemaF, Seed]): CoalgebraM[Gen, SchemaF, Seed] = CoalgebraM { seed =>
    alg(seed).flatMap(schema => Gen.oneOf(schema, schema.withAttributes(Optional.key -> true)))
  }

  def using(coalg: CoalgebraM[Gen, SchemaF, Seed] = default.coalgebra): Seed => Gen[Schema] = {
    val fn = scheme.anaM(coalg)
    val result: Seed => Gen[Schema] = { seed =>
      fn(seed)
    }
    result
  }
}
