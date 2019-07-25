package datum.avrolib.data
import datum.avrolib.schemas.{AvroSchemaReader, AvroSchemaWriter}
import datum.patterns.data
import datum.patterns.data._
import datum.patterns.schemas._
import higherkindness.droste.data.{AttrF, Fix}
import higherkindness.droste.data.prelude._
import higherkindness.droste.{Algebra, Coalgebra, scheme}
import org.apache.avro.generic.GenericData.Record
import org.apache.avro.generic.GenericRecord
import org.apache.avro.{SchemaBuilder, Schema => AvroSchema}
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

    def extract(tpe: Type, d: DataF[Data], isOpt: Boolean): Any = {
      (tpe, d) match {
        case (IntType, data.IntValue(v))         => v
        case (TextType, data.TextValue(v))       => v
        case (LongType, data.LongValue(v))       => v
        case (BooleanType, data.BooleanValue(v)) => v
        case (DoubleType, data.DoubleValue(v))   => v
        case (FloatType, data.FloatValue(v))     => v
        case (_, data.EmptyValue) if isOpt       => null
        case (x, _)                              => throw new Exception(s"Not handled: $x")
      }
    }

    Algebra[SchemaWithAvro, Data => Any] {

      case AttrF(None, ValueF(tpe, _)) =>
        d =>
          extract(tpe, Fix.un[DataF](d), false) //todo: isOpt

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

          case otherwise => ???
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
        }

      case AttrF(Some(avro), NamedUnionF(alts, _)) =>
        Fix.un[DataF](_) match {
          case NamedUnionValue(selection, value) =>
            val selected = avro.getTypes.asScala.find { s =>
              val name = s.getProp(datum.avrolib.schemas.ORIGINAL_NAME_KEY) match {
                case null => s.getName
                case orig => orig
              }
              name == selection
            }
            val generic = new Record(selected.get)
            generic.put("schema", alts(selection)(value))
            generic
        }

      case AttrF(_, otherwise) =>
        assert(false, s"TODO: ${pprint.apply(otherwise)}")
        ???
    }
  }

  def generateFor(schema: Schema): Data => GenericRecord = {
    // Functor for AttrF[...] comes from 'import higherkindness.droste.data.prelude._'
    val annotated = scheme.ana(annotate).apply(schema)
    val fn = scheme.cata(algebra).apply(annotated)
    fn.asInstanceOf[Data => GenericRecord]
  }
}
