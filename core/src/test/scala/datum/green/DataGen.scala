package datum.green
import java.time.ZonedDateTime

import datum.green.patterns.data
import datum.green.patterns.data.Data
import datum.green.patterns.schemas._
import com.fortysevendeg.scalacheck.datetime.jdk8.ArbitraryJdk8._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import qq.droste.{AlgebraM, scheme}

object DataGen {

  val dataGen: AlgebraM[Gen, SchemaF, Data] = AlgebraM {
    case StructF(fields, _) => data.struct(fields)
    case RowF(elements, _)  => data.row(elements.map(_.value))

    case ValueF(IntType, _)     => arbitrary[Int].map(data.integer)
    case ValueF(LongType, _)    => arbitrary[Long].map(data.long)
    case ValueF(FloatType, _)   => arbitrary[Float].map(data.float)
    case ValueF(DoubleType, _)  => arbitrary[Double].map(data.double)
    case ValueF(TextType, _)    => arbitrary[String].map(data.text)
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

    case ohno =>
      println(s"======================= FAILED TO MATCH: ${ohno}")
      ???

  }

  def generatorFor(schema: Schema): Gen[Data] = {
    val fn = scheme.cataM(dataGen)
    fn(schema)
  }
}
