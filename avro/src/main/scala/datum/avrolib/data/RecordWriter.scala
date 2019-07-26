package datum.avrolib.data
import java.time.format.DateTimeFormatter

import datum.avrolib.schemas.AvroSchemaWriter
import datum.modifiers.Optional
import datum.patterns.data
import datum.patterns.data._
import datum.patterns.schemas._
import datum.patterns.properties._
import higherkindness.droste.data.{AttrF, Fix}
import higherkindness.droste.data.prelude._
import higherkindness.droste.{Algebra, Coalgebra, scheme}
import org.apache.avro.generic.GenericData.Record
import org.apache.avro.generic.GenericRecord
import org.apache.avro.{Schema => AvroSchema}
import org.apache.avro.generic.GenericRecordBuilder

import scala.collection.JavaConverters._

object RecordWriter {

  type SchemaWithAvro[A] = AttrF[SchemaF, Option[AvroSchema], A]

  val annotate: Coalgebra[SchemaWithAvro, Schema] = Coalgebra { schema =>
    Fix.un[SchemaF](schema) match {
      case obj @ ObjF(_, _) =>
        val avro = AvroSchemaWriter.write(schema)
        AttrF(Some(avro), obj)

      case obj @ RowF(_, _) =>
        val avro = AvroSchemaWriter.write(schema)
        AttrF(Some(avro), obj)

      case indexed @ IndexedUnionF(_, _) =>
        val avro = AvroSchemaWriter.write(schema)
        AttrF(Some(avro), indexed)

      case named @ NamedUnionF(_, _) =>
        val avro = AvroSchemaWriter.write(schema)
        AttrF(Some(avro), named)

      case otherwise => AttrF(None, otherwise)
    }
  }

  val algebra: Algebra[SchemaWithAvro, Data => Any] = {

    def extract(tpe: Type, d: DataF[Data]): Any = {
      (tpe, d) match {
        case (IntType, data.IntValue(v))                 => v
        case (TextType, data.TextValue(v))               => v
        case (LongType, data.LongValue(v))               => v
        case (BooleanType, data.BooleanValue(v))         => v
        case (DoubleType, data.DoubleValue(v))           => v
        case (FloatType, data.FloatValue(v))             => v
        case (DateType, data.DateValue(v))               => v.format(DateTimeFormatter.ISO_LOCAL_DATE)
        case (TimestampType, data.TimestampValue(v))     => v.getEpochSecond * 1000 //expects millis
        case (DateTimeType, data.DateTimeValue(v))       => v.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
        case (ZonedDateTimeType, data.ZonedTimeValue(v)) => v.format(DateTimeFormatter.ISO_ZONED_DATE_TIME)
        case (BytesType, data.BytesValue(v))             => java.nio.ByteBuffer.wrap(v)
        case (_, data.EmptyValue)                        => null
        case (x, _)                                      => throw new Exception(s"Not handled: $x")
      }
    }

    Algebra[SchemaWithAvro, Data => Any] {

      case AttrF(None, ValueF(tpe, _)) =>
        d =>
          extract(tpe, Fix.un[DataF](d))

      case AttrF(Some(avro), ObjF(fields, _)) =>
        Fix.un[DataF](_) match {
          case ObjValue(values) =>
            val generic = new GenericRecordBuilder(avro)

            // Have to zip with avro fields because fields may have been renamed
            avro.getFields.asScala.zip(fields).foreach {
              case (avroField, (key, fn)) =>
                val d = values.getOrElse(key, empty)
                generic.set(avroField, fn(d))
            }
            generic.build()

          case otherwise =>
            assert(false, "FWWWWWWWWWWWWWWWWWWWWWWWWW")
            ???
        }

      case AttrF(Some(avro), RowF(columns, props)) =>
        Fix.un[DataF](_) match {
          case RowValue(values) =>
            val generic = new GenericRecordBuilder(avro)
            val records = columns.zip(values).map {
              case (col, v) =>
                col.value.apply(v)
            }
            avro.getFields.asScala.zip(records).foreach {
              case (avroField, record) =>
                generic.set(avroField, record)
            }
            generic.build()

          case otherwise => ???
        }

      case AttrF(Some(avro), IndexedUnionF(alts, _)) =>
        Fix.un[DataF](_) match {
          case IndexedUnionValue(idx, value) =>
            val selected = avro.getTypes.asScala(idx)
            val generic = new Record(selected)
            generic.put("schema", alts(idx)(value))
            generic
          case otherwise => ???
        }

      case AttrF(None, ArrayF(conforms, _)) =>
        Fix.un[DataF](_) match {
          case RowValue(values) =>
            values.map(conforms).asJava
          case otherwise => ???
        }

      case AttrF(Some(avro), NamedUnionF(alts, _)) =>
        Fix.un[DataF](_) match {
          case NamedUnionValue(selection, value) =>
            // NamedUnion record must be written with the original name set as a prop
            val selected = avro.getTypes.asScala.find { s =>
              selection == s.getProp(datum.avrolib.schemas.ORIGINAL_NAME_KEY)
            }
            val generic = new Record(selected.get)
            generic.put("schema", alts(selection)(value))
            generic

          case otherwise => ???
        }

      case AttrF(_, otherwise) =>
        assert(false, s"TODO: ${pprint.apply(otherwise)}")
        ???
    }
  }

  def optional(algebra: Algebra[SchemaWithAvro, Data => Any]): Algebra[SchemaWithAvro, Data => Any] =
    Algebra[SchemaWithAvro, Data => Any] {

      // Avro uses unions to create optional/nullable values - unwrap them to correctly handle options
      case AttrF(Some(avro), obj @ ObjF(_, _)) if avro.getType == AvroSchema.Type.UNION =>
        assert(obj.properties.get(Optional.key).contains(true.prop), "Expected an optional object!")
        assert(avro.getTypes.size() == 2, "Unexpected UNION size for an optional obj!")
        assert(avro.getTypes.get(1).getType == AvroSchema.Type.NULL, "NULL part of union not in expected position!")
        val unwarpped = avro.getTypes.get(0)

        {
          case data.empty => null
          case otherwise  => algebra(AttrF(Some(unwarpped), obj))(otherwise)
        }

      case AttrF(Some(avro), row @ RowF(_, _)) if avro.getType == AvroSchema.Type.UNION =>
        assert(row.properties.get(Optional.key).contains(true.prop), "Expected an optional row!")
        assert(avro.getTypes.size() == 2, "Unexpected UNION size for an optional row!")
        assert(avro.getTypes.get(1).getType == AvroSchema.Type.NULL, "NULL part of union not in expected position!")
        val unwarpped = avro.getTypes.get(0)

        {
          case data.empty => null
          case otherwise  => algebra(AttrF(Some(unwarpped), row))(otherwise)
        }

      case AttrF(Some(avro), indexed @ IndexedUnionF(alts, _))
          if indexed.properties.get(Optional.key).contains(true.prop) =>
        Fix.un[DataF](_) match {
          case IndexedUnionValue(idx, value) =>
            // a null was prepended to the union of alternatives
            // this makes sure to account for that
            val selected = avro.getTypes.asScala(idx + 1)
            val generic = new Record(selected)
            generic.put("schema", alts(idx)(value))
            generic

          case EmptyValue => null

          case otherwise => ???
        }

      case other @ AttrF(_, schema) if schema.properties.get(Optional.key).contains(true.prop) => {
        case data.empty => null
        case otherwise  => algebra(other)(otherwise)
      }

      case otherwise => algebra(otherwise)
    }

  def generateFor(schema: Schema): Data => GenericRecord = {
    // Functor for AttrF[...] comes from 'import higherkindness.droste.data.prelude._'
    val annotated = scheme.ana(annotate).apply(schema)
    val fn = scheme.cata(algebra).apply(annotated)
    fn.asInstanceOf[Data => GenericRecord]
  }

  def define(algebra: Algebra[SchemaWithAvro, Data => Any]): Schema => Data => GenericRecord = { schema =>
    val annotated = scheme.ana(annotate).apply(schema)
    val fn = scheme.cata(algebra).apply(annotated)
    fn.asInstanceOf[Data => GenericRecord]
  }
}
