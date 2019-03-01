package datum.gen.algebras

import java.time.ZonedDateTime

import datum.patterns.data
import datum.patterns.data.Data
import datum.patterns.schemas._
import com.fortysevendeg.scalacheck.datetime.jdk8.ArbitraryJdk8._
import datum.modifiers.Optional
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import qq.droste.{AlgebraM, scheme}

object DataGen {

  val algebra: AlgebraM[Gen, SchemaF, Data] = AlgebraM {
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

    case ValueF(InstantType, _) =>
      arbitrary[ZonedDateTime].map { zdt =>
        data.instant(zdt.toInstant)
      }

    case ValueF(LocalTimeType, _) =>
      arbitrary[ZonedDateTime].map { zdt =>
        data.localTime(zdt.toLocalDateTime)
      }

    case ValueF(ZonedTimeType, _) => arbitrary[ZonedDateTime].map(data.zonedTime)

    case ObjF(fields, _) =>
      Gen.chooseNum(0.0, 1.0).map { x =>
        data.obj(fields.filter {
          case (_, data.empty) => x <= 0.5
          case _               => true
        })
      }

    case RowF(elements, _) => data.row(elements.map(_.value))

    case UnionF(alternatives, _) => Gen.oneOf(alternatives)

    case ohno =>
      println(s"======================= FAILED TO MATCH: ${ohno}")
      ???

  }

  def optional(alg: AlgebraM[Gen, SchemaF, Data]): AlgebraM[Gen, SchemaF, Data] = AlgebraM { schema =>
    if (schema.attributes.contains(Optional.key)) {
      Gen.frequency(1 -> Gen.const(data.empty), 5 -> alg(schema))
    } else {
      alg(schema)
    }
  }

  def using(alg: AlgebraM[Gen, SchemaF, Data] = algebra): Schema => Gen[Data] = {
    val fn = scheme.cataM(alg)
    val result: Schema => Gen[Data] = { s =>
      fn(s)
    }
    result
  }
}
