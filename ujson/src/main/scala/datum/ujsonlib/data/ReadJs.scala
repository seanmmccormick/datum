package datum.ujsonlib.data
import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.Base64

import cats.Traverse
import datum.patterns.data._
import datum.patterns.{schemas, data => d}
import datum.patterns.schemas._
import qq.droste.{Algebra, Coalgebra, scheme}
import cats.syntax.either._
import cats.instances.either._
import cats.instances.vector._
import cats.data.Chain
import datum.modifiers.Optional
import ujson.Js

import scala.collection.immutable.SortedMap

object ReadJs {

  private def fromJs(tpe: schemas.Type, js: Js.Value): Either[String, Data] = {
    (tpe, js) match {
      case (IntType, Js.Num(v))      => Right(d.integer(v.toInt))
      case (LongType, Js.Str(v))     => Either.catchNonFatal(d.long(v.toLong)).leftMap(_ => "Invalid Long")
      case (FloatType, Js.Num(v))    => Right(d.float(v.toFloat))
      case (DoubleType, Js.Num(v))   => Right(d.double(v))
      case (TextType, Js.Str(v))     => Right(d.text(v))
      case (BooleanType, Js.Bool(v)) => Right(d.boolean(v))

      case (BytesType, Js.Str(v)) =>
        Either
          .catchNonFatal {
            d.bytes(Base64.getDecoder.decode(v))
          }
          .leftMap(_ => "Invalid Bytes")

      case (DateType, Js.Str(v)) =>
        Either
          .catchNonFatal {
            d.date(LocalDate.parse(v))
          }
          .leftMap(_ => "Invalid DateType")

      case (TimestampType, Js.Str(v)) =>
        Either
          .catchNonFatal {
            d.timestamp(Instant.parse(v))
          }
          .leftMap(_ => "Invalid Timestamp")

      case (DateTimeType, Js.Str(v)) =>
        Either
          .catchNonFatal {
            d.localTime(LocalDateTime.parse(v))
          }
          .leftMap(_ => "Invalid DateTime")

      case (ZonedDateTimeType, Js.Str(v)) =>
        Either
          .catchNonFatal {
            d.zonedTime(ZonedDateTime.parse(v))
          }
          .leftMap(_ => "Invalid ZonedDateTime")

      case err => Left(s"Expected: ${schemas.Type.asString(tpe)} but got ${pprint.apply(err, 2)}")
    }
  }

  val algebra: Algebra[SchemaF, Js.Value => Either[String, Data]] = Algebra {
    case ValueF(tpe, _) =>
      js =>
        fromJs(tpe, js)

    case ObjF(schema, _) => {
      case Js.Obj(fields) =>
        val values = schema.foldLeft(Chain.empty[Either[String, (String, Data)]]) {
          case (acc, (k, fn)) =>
            acc.prepend(fn(fields.getOrElse(k, Js.Null)).map((k, _)))
        }
        Traverse[Chain].sequence(values).map { collected =>
          val builder = SortedMap.newBuilder[String, Data]
          collected.iterator.foreach { case (k, d) => builder += k -> d }
          d.obj(builder.result())
        }

      case _ => Left("todo: ObjF")
    }

    case RowF(elements, _) => {
      case Js.Arr(values) =>
        val valuesThenNulls = elements.zip(values.toStream #::: Stream.continually(Js.Null))
        val results = Traverse[Vector].traverse(valuesThenNulls) {
          case (col, v) =>
            col.value.apply(v)
        }
        results.map(d.row)

      case _ => Left("todo: RowF")
    }

    case ArrayF(fn, _) => {
      case Js.Arr(values) =>
        Traverse[Vector].traverse(values.toVector)(fn).map(d.row)
      case _ =>
        Left("todo: ArrayF")
    }

    case UnionF(unionFns, _) => {
      case Js.Arr(z) if z.length == 2 =>
        val tag = z(0).num.toInt
        val value = z(1)
        if (tag != -1 && tag < unionFns.length) {
          println(s"GOOD $tag")
          unionFns(tag)(value)
        } else {
          println("BAD")
          Left("Invalid Union")
        }
      case nope =>
        Left("todo: UnionF")
    }
  }

  def optional(
    alg: Algebra[SchemaF, Js.Value => Either[String, Data]]
  ): Algebra[SchemaF, Js.Value => Either[String, Data]] = Algebra { schema => js =>
    val fn = alg(schema)
    fn(js) match {
      case Left(err) if schema.attributes.contains(Optional.key) => Right(d.empty)
      case otherwise                                             => otherwise
    }
  }

  def define(
    alg: Algebra[SchemaF, Js.Value => Either[String, Data]] = algebra
  ): Schema => Js.Value => Either[String, Data] = {
    scheme.cata(alg)
  }
}
