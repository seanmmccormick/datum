package datum.ujsonlib.data
import java.time.format.DateTimeFormatter
import java.time.temporal.{ChronoField, TemporalField}

import datum.patterns.{data, schemas}
import datum.patterns.data.{Data, DataF}
import datum.patterns.schemas._
import qq.droste.{Algebra, scheme}
import qq.droste.syntax.project._
import ujson.Js

import scala.collection.mutable

object WriteJs {

  private def toJs(tpe: schemas.Type, d: DataF[Data]): Js.Value = {
    (tpe, d) match {
      case (TextType, data.TextValue(v))           => Js.Str(v)
      case (BooleanType, data.BooleanValue(v))     => Js.Bool(v)
      case (IntType, data.IntValue(v))             => Js.Num(v)
      case (LongType, data.LongValue(v))           => Js.Str(v.toString)
      case (TimestampType, data.TimestampValue(v)) => Js.Str(v.toString)

      case err =>
        assert(false, pprint.apply(err))
        Js.Null
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
  }

  def define(using: Algebra[SchemaF, Data => Js.Value] = algebra): Schema => Data => Js.Value = {
    scheme.cata(using)
  }
}
