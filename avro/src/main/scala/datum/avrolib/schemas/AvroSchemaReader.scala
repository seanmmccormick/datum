package datum.avrolib.schemas
import datum.patterns.schemas._
import datum.avrolib.properties._
import datum.patterns.properties.Property
import higherkindness.droste.{Coalgebra, scheme}
import org.apache.avro.LogicalTypes.TimestampMillis
import org.apache.avro.util.internal.JacksonUtils
import org.apache.avro.{Schema => AvroSchema}

import scala.collection.JavaConverters._
import scala.collection.immutable.SortedMap

object AvroSchemaReader {

  val coalgebra: Coalgebra[SchemaF, AvroSchema] = Coalgebra[SchemaF, AvroSchema] { avro =>
    avro.getType match {
      case AvroSchema.Type.INT     => ValueF(IntType, extractProps(avro))
      case AvroSchema.Type.FLOAT   => ValueF(FloatType, extractProps(avro))
      case AvroSchema.Type.DOUBLE  => ValueF(DoubleType, extractProps(avro))
      case AvroSchema.Type.BOOLEAN => ValueF(BooleanType, extractProps(avro))
      case AvroSchema.Type.STRING =>
        avro.getProp(RECORD_TYPE_KEY) match {
          case "date"            => ValueF(DateType, extractProps(avro))
          case "date-time"       => ValueF(DateTimeType, extractProps(avro))
          case "zoned-date-time" => ValueF(ZonedDateTimeType, extractProps(avro))
          case _                 => ValueF(TextType, extractProps(avro))
        }
      case AvroSchema.Type.LONG =>
        avro.getLogicalType match {
          case _: TimestampMillis => ValueF(TimestampType, extractProps(avro))
          case _                  => ValueF(LongType, extractProps(avro))
        }
      case AvroSchema.Type.BYTES => ValueF(BytesType, extractProps(avro))

      case AvroSchema.Type.RECORD =>
        val fields = avro.getFields.asScala
        avro.getProp(RECORD_TYPE_KEY) match {

          // If RECORD_TYPE_KEY is not set, default to using an Obj for the Schema
          case "obj" | null =>
            val objFields = SortedMap.newBuilder[String, AvroSchema]
            fields.foreach { field =>
              objFields += (originalName(field) -> field.schema())
            }
            ObjF(objFields.result(), extractProps(avro))

          case "row" =>
            val columns = fields.view.map { field =>
              // Avro Union types can't carry properties despite being a (avro) "Schema" :/
              //
              // As done on the write side, props are instead carried by a fake union type appended to the
              // end of the list of alternatives
              val noHeader = if (field.schema().isUnion) {
                field.schema().getTypes.asScala.last.getObjectProp(NO_HEADER).asInstanceOf[Boolean]
              } else {
                field.schema().getObjectProp(NO_HEADER).asInstanceOf[Boolean]
              }

              val header =
                if (noHeader) None
                else Some(originalName(field))

              Column(field.schema(), header)
            }.toVector

            RowF(columns, extractProps(avro))

          case _ => ???
        }

      case AvroSchema.Type.ARRAY =>
        ArrayF(avro.getElementType, extractProps(avro))

      case AvroSchema.Type.UNION =>
        val fields = avro.getTypes.asScala
        val propertyRecord = fields.last
        val alternatives = fields.take(fields.length - 1)

        propertyRecord.getProp(RECORD_TYPE_KEY) match {
          case "named" =>
            val alts = SortedMap.newBuilder[String, AvroSchema]
            alternatives.foreach { alt =>
              val original = alt.getProp(ORIGINAL_NAME_KEY) match {
                case null => alt.getName
                case orig => orig
              }
              alts += (original -> alt.getField("schema").schema())
            }
            UnionF(alts.result(), extractProps(propertyRecord))
        }

      case todo => ???
    }
  }

  // These are internal properties that should be filtered out
  private val filter = Set(
    RECORD_TYPE_KEY,
    ORIGINAL_NAME_KEY,
    NO_HEADER
  )

  private def extractProps(avro: AvroSchema) = {
    val props = SortedMap.newBuilder[String, Property]
    avro.getObjectProps.asScala.foreach {
      case (k, v) =>
        if (!filter.contains(k)) {
          props += (k -> JacksonUtils.toJsonNode(v).toDatum)
        }
    }
    props.result()
  }

  def originalName(field: AvroSchema.Field) = {
    if (field.schema().isUnion) {
      field.schema().getTypes.asScala.last.getProp(ORIGINAL_NAME_KEY) match {
        case null => field.name()
        case orig => orig
      }
    } else {
      field.schema().getProp(ORIGINAL_NAME_KEY) match {
        case null => field.name()
        case orig => orig
      }
    }
  }

  private val fromAvroSchemaFn = scheme.ana(coalgebra)

  def read(schema: AvroSchema): Schema = {
    fromAvroSchemaFn(schema)
  }

  def using(coalgebra: Coalgebra[SchemaF, AvroSchema]): AvroSchema => Schema = {
    val fn = scheme.ana(coalgebra)
    avro =>
      fn(avro)
  }
}
