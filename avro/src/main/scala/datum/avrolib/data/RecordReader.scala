package datum.avrolib.data

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}
import java.time.format.DateTimeFormatter

import datum.patterns.data
import datum.patterns.data.Data
import datum.patterns.schemas._
import higherkindness.droste.{Algebra, scheme}
import org.apache.avro.generic.{GenericData, GenericRecord}
import org.apache.avro.util.Utf8

import scala.collection.immutable.SortedMap
import scala.collection.JavaConverters._

object RecordReader {

  // TODO - REAL ERROR HANDLING!

  val algebra: Algebra[SchemaF, AnyRef => Data] = {

    def asData(tpe: Type, any: Any): Data = {
      (tpe, any) match {
        case (_, null)                 => data.empty
        case (IntType, v: Int)         => data.integer(v)
        case (TextType, v: Utf8)       => data.text(v.toString)
        case (BooleanType, v: Boolean) => data.boolean(v)
        case (LongType, v: Long)       => data.long(v)
        case (FloatType, v: Float)     => data.float(v)
        case (DoubleType, v: Double)   => data.double(v)

        case (DateType, v: Utf8) =>
          val date = LocalDate.parse(v, DateTimeFormatter.ISO_LOCAL_DATE)
          data.date(date)

        case (TimestampType, v: Long) =>
          data.timestamp(Instant.ofEpochMilli(v))

        case (DateTimeType, v: Utf8) =>
          val date = LocalDateTime.parse(v, DateTimeFormatter.ISO_LOCAL_DATE_TIME)
          data.localTime(date)

        case (ZonedDateTimeType, v: Utf8) =>
          val date = ZonedDateTime.parse(v, DateTimeFormatter.ISO_ZONED_DATE_TIME)
          data.zonedTime(date)

        case (BytesType, v: java.nio.ByteBuffer) =>
          val copy = Array.ofDim[Byte](v.remaining())
          v.get(copy)
          data.bytes(copy)

        case (_, other) =>
          println(s"FAILED TO MATCH: $other")
          println(other.getClass)
          ???
      }
    }

    Algebra {
      case ValueF(tpe, _) =>
        any =>
          asData(tpe, any)

      case ObjF(fields, _) =>
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

      case RowF(columns, _) => {
        case generic: GenericRecord =>
          // As in the case with Obj, zip with the avro fields to get the correct name
          val values = columns.zip(generic.getSchema.getFields.asScala).map {
            case (col, avroField) =>
              col.value.apply(generic.get(avroField.name()))
          }
          data.row(values)
        case _ => ???
      }

      case ArrayF(conforms, _) => {
        case array: GenericData.Array[AnyRef] =>
          val values = array.iterator().asScala.map[Data](conforms)
          data.array(values.toVector)
        case _ => ???
      }

      case NamedUnionF(alts, _) => {
        case generic: GenericRecord =>
          val name = generic.getSchema.getProp(datum.avrolib.schemas.ORIGINAL_NAME_KEY)
          data.union(name, alts(name)(generic.get("schema")))

        case _ => ???
      }

      case IndexedUnionF(alts, _) => {
        case generic: GenericRecord =>
          val idx = generic.getSchema.getObjectProp(datum.avrolib.schemas.ORIGINAL_NAME_KEY).asInstanceOf[Int]
          data.indexed(idx, alts(idx)(generic.get("schema")))
        case _ => ???
      }

      case otherwise =>
        assert(false, s"TODOOO: READ RECOOORD: $otherwise")
        ???
    }
  }

  def generateFor(schema: Schema): GenericRecord => Data = {
    val fn = scheme.cata(algebra).apply(schema)
    fn.asInstanceOf[GenericRecord => Data]
  }
}
