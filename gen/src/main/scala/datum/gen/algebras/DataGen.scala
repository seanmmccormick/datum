package datum.gen.algebras

import java.time.ZonedDateTime

import cats.Traverse
import datum.patterns.data
import datum.patterns.data.Data
import datum.patterns.schemas._
import com.fortysevendeg.scalacheck.datetime.jdk8.ArbitraryJdk8._
import datum.modifiers.Optional
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import qq.droste.{Algebra, AlgebraM, scheme}

object DataGen {

  private val helpers: AlgebraM[Gen, SchemaF, Data] = AlgebraM {
    case ObjF(fields, _) =>
      Gen.chooseNum(0.0, 1.0).map { x =>
        data.obj(fields.filter {
          case (_, data.empty) => x <= 0.5
          case _               => true
        })
      }

    case RowF(elements, _) => data.row(elements.map(_.value))

    case err => throw new Exception(s"Impossible: ${pprint.apply(err)}")
  }

  val algebra: Algebra[SchemaF, Gen[Data]] = Algebra {
    case ValueF(IntType, _)     => arbitrary[Int].map(data.integer)
    case ValueF(LongType, _)    => arbitrary[Long].map(data.long)
    case ValueF(FloatType, _)   => arbitrary[Float].map(data.float)
    case ValueF(DoubleType, _)  => arbitrary[Double].map(data.double)
    case ValueF(TextType, _)    => Gen.asciiPrintableStr.map(data.text)
    case ValueF(BooleanType, _) => arbitrary[Boolean].map(data.boolean)
    case ValueF(BytesType, _)   => arbitrary[Array[Byte]].map(data.bytes)

    case ValueF(DateType, _) =>
      arbitrary[ZonedDateTime].map { zdt =>
        data.date(zdt.toLocalDate)
      }

    case ValueF(TimestampType, _) =>
      arbitrary[ZonedDateTime].map { zdt =>
        data.instant(zdt.toInstant)
      }

    case ValueF(DateTimeType, _) =>
      arbitrary[ZonedDateTime].map { zdt =>
        data.localTime(zdt.toLocalDateTime)
      }

    case ValueF(ZonedDateTimeType, _) => arbitrary[ZonedDateTime].map(data.zonedTime)

    case ArrayF(element, _) =>
      Gen.resize(5, Gen.containerOf[Vector, Data](element).map(data.row))

    case UnionF(alternatives, _) =>
      Gen.chooseNum(0, alternatives.length - 1).flatMap {
        alternatives(_)
      }

    case other =>
      Traverse[SchemaF].sequence(other).flatMap(helpers(_))
  }

  def optional(alg: Algebra[SchemaF, Gen[Data]]): Algebra[SchemaF, Gen[Data]] = Algebra { schema =>
    if (schema.attributes.contains(Optional.key)) {
      Gen.frequency(1 -> Gen.const(data.empty), 5 -> alg(schema))
    } else {
      alg(schema)
    }
  }

  def define(alg: Algebra[SchemaF, Gen[Data]] = algebra): Schema => Gen[Data] = {
    val fn = scheme.cata(alg)
    val result: Schema => Gen[Data] = { s =>
      fn(s)
    }
    result
  }
}
