package datum.ujsonlib.data

import java.util.Base64
import java.time.format.DateTimeFormatter
import java.time.ZoneId

import datum.patterns.{data, schemas}
import datum.patterns.data.{Data, DataF}
import datum.patterns.schemas._
import qq.droste.{Algebra, RAlgebra, scheme}
import qq.droste.syntax.project._
import ujson.Js

import scala.collection.mutable

object WriteJs {

  private def toJs(tpe: schemas.Type, d: DataF[Data]): Js.Value = {
    (tpe, d) match {
      case (IntType, data.IntValue(v))             => Js.Num(v)
      case (LongType, data.LongValue(v))           => Js.Str(v.toString)
      case (FloatType, data.FloatValue(v))         => Js.Num(v)
      case (DoubleType, data.DoubleValue(v))       => Js.Num(v)
      case (TextType, data.TextValue(v))           => Js.Str(v)
      case (BooleanType, data.BooleanValue(v))     => Js.Bool(v)
      case (BytesType, data.BytesValue(v)) =>
        val encoded = Base64.getEncoder.encodeToString(v)
        Js.Str(encoded)
      case (DateType, data.DateValue(v)) =>
        Js.Str(v.format(DateTimeFormatter.ISO_LOCAL_DATE))
      case (TimestampType, data.TimestampValue(v)) =>
        Js.Str(v.atZone(ZoneId.of("UTC")).format(DateTimeFormatter.ISO_INSTANT))
      case (DateTimeType, data.DateTimeValue(v)) =>
        Js.Str(v.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
      case (ZonedDateTimeType, data.ZonedTimeValue(v)) =>
        Js.Str(v.format(DateTimeFormatter.ISO_ZONED_DATE_TIME))
      case _ => Js.Null
    }
  }

  val algebra: Algebra[SchemaF, Data => Js.Value] = Algebra {
    case ValueF(tpe, _) =>
      d =>
        toJs(tpe, d.project)

    case ObjF(schema, _) =>
      _.project match {
        case data.ObjValue(fields) =>
          val jsFields = mutable.LinkedHashMap.empty[String, Js.Value]
          val has = fields.keySet.intersect(schema.keySet)
          val missing = schema.keySet.diff(fields.keySet)

          has.foreach { k =>
            jsFields.put(k, schema(k)(fields(k)))
          }

          missing.foreach { k =>
            jsFields.put(k, schema(k)(data.empty))
          }

          Js.Obj(jsFields)

        case _ => Js.Null
      }

    case RowF(elements, _) =>
      _.project match {
        case data.RowValue(rows) =>
          val empty = Stream.continually(data.empty)
          elements.zip(rows.toStream #::: empty).map {
            case (col, d) => col.value.apply(d)
          }

        case _ => Js.Null
      }

    case ArrayF(fn, _) =>
      _.project match {
        case data.RowValue(values) => values.map(fn)
        case _                     => Js.Null
      }

    case UnionF(options, _) =>
      d =>
        val iter = options.toIterator
        var result: Js.Value = Js.Null
        var idx = -1
        while (iter.hasNext && result == Js.Null) {
          val fn = iter.next()
          result = fn(d)
          idx += 1
        }
        if (idx == options.length) idx = -1
        Js.Arr(Js.Num(idx), result)

    case todo =>
      println("TODO! " + todo)
      _ =>
        Js.Null
  }

  def define(using: Algebra[SchemaF, Data => Js.Value] = algebra): Schema => Data => Js.Value = {
    scheme.cata(using)
  }
}
