package datum.avrolib.data

import datum.patterns.data
import datum.patterns.data.Data
import datum.patterns.schemas._
import higherkindness.droste.{Algebra, scheme}

import org.apache.avro.generic.GenericRecord
import org.apache.avro.util.Utf8

import scala.collection.immutable.SortedMap
import scala.collection.JavaConverters._

object RecordReader {

  val algebra: Algebra[SchemaF, AnyRef => Data] = {

    def asData(tpe: Type, any: Any): Data = {
      (tpe, any) match {
        case (_, null)                 => data.empty
        case (IntType, v: Int)         => data.integer(v)
        case (TextType, v: Utf8)       => data.text(v.toString)
        case (BooleanType, v: Boolean) => data.boolean(v)
        case (_, other) =>
          println(s"FAILED TO MATCH: $other")
          println(other.getClass)
          ???
      }
    }

    Algebra {
      case ObjF(fields, prop) =>
        any =>
          val generic = any.asInstanceOf[GenericRecord]

          val builder = SortedMap.newBuilder[String, Data]

          // Have to zip with the record's schema because the (avro) field name may not match the (datum) schema name
          // However, order is preserved, so we can simply zip them together
          fields.iterator.zip(generic.getSchema.getFields.asScala.iterator).foreach {
            case ((key, fn), avroField) =>
              builder += key -> fn(generic.get(avroField.name()))
          }

          data.obj(builder.result())

      case ValueF(tpe, _) =>
        any =>
          asData(tpe, any)

      case otherwise =>
        assert(false, "TODOOO: READ RECOOORD")
        ???
    }
  }

  def generateFor(schema: Schema): GenericRecord => Data = {
    val fn = scheme.cata(algebra).apply(schema)
    fn.asInstanceOf[GenericRecord => Data]
  }
}
