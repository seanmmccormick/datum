package datum.avrolib.schemas
import cats.data.State
import datum.patterns.schemas._
import datum.avrolib.properties._
import datum.patterns.properties.Property
import higherkindness.droste.{AlgebraM, Coalgebra, scheme}
import org.apache.avro.util.internal.JacksonUtils
import org.apache.avro.{SchemaBuilder, Schema => AvroSchema}

import scala.collection.JavaConverters._
import scala.collection.immutable.SortedMap
import scala.collection.mutable

object SchemaReadWriter {

  type Registry[A] = State[Map[Int, AvroSchema], A]

  // Used to encode original (datum) schema type
  private val RECORD_TYPE_KEY = "datum.record.type"

  // Used to encode the original name of a given field or header
  private val ORIGINAL_NAME_KEY = "datum.original.name"

  // Used to encode if a header was never set
  private val NO_HEADER = "datum.column.noheader"

  def includeProps(avro: AvroSchema, props: PropertyMap): AvroSchema = {
    props.foreach {
      case (k, prop) =>
        avro.addProp(k, prop.toAvro)
    }
    avro
  }

  val algebra: AlgebraM[Registry, SchemaF, AvroSchema] = {

    def primitive(typ: AvroSchema.Type, props: PropertyMap): Registry[AvroSchema] = {
      State.pure {
        val result = AvroSchema.create(typ)
        includeProps(result, props)
      }
    }

    // Helper function to ensure only valid (no special characters, no duplicates, etc..) names
    def safeName(inp: String): (String, Boolean) = {
      val builder = mutable.StringBuilder.newBuilder
      var modified = false

      inp.iterator.foreach {
        case x if x.isLetterOrDigit =>
          builder.append(x)

        case _ =>
          builder.append('_')
          modified = true
      }

      val result = builder.result()
      if (result == "") ("Unnamed", true)
      else if (!(result.head.isLetter || result.head == '_')) (s"_$result", true)
      else (result, modified)
    }

    // Helper function to ensure in addition to safeName, duplicate field names are also handled correctly.
    // Original name is stored in ORIGINAL_NAME_KEY as an avro property, if it is needed
    def safeFieldName(field: AvroSchema, original: String, idx: Int, seen: mutable.Map[String, Int]): String = {

      def next(check: String): String = {
        val result =
          if (seen(check) == 0) check
          else next(s"$check${seen(check) + 1}")
        seen(check) += 1
        result
      }

      val (safe, modified) = safeName(original)

      if (modified)
        field.addProp(ORIGINAL_NAME_KEY, original)

      if (seen(safe) == 0) {
        seen(safe) += 1
        safe
      } else {
        if (!modified) // we can only set this property once
          field.addProp(ORIGINAL_NAME_KEY, original)
        next(safe)
      }
    }

    AlgebraM {
      case ValueF(IntType, props)     => primitive(AvroSchema.Type.INT, props)
      case ValueF(LongType, props)    => primitive(AvroSchema.Type.LONG, props)
      case ValueF(FloatType, props)   => primitive(AvroSchema.Type.FLOAT, props)
      case ValueF(DoubleType, props)  => primitive(AvroSchema.Type.DOUBLE, props)
      case ValueF(BooleanType, props) => primitive(AvroSchema.Type.BOOLEAN, props)
      case ValueF(TextType, props)    => primitive(AvroSchema.Type.STRING, props)
      case ValueF(BytesType, props)   => primitive(AvroSchema.Type.BYTES, props)

      case ObjF(fields, props) =>
        val fingerprint = fields.hashCode()
        State { registry =>
          if (registry.contains(fingerprint)) {
            (registry, registry(fingerprint))
          } else {
            val seen = mutable.Map.empty[String, Int].withDefaultValue(0)
            val avro = fields
              .foldLeft(SchemaBuilder.record("r%x".format(fingerprint)).fields()) {
                case (acc, (k, v)) =>
                  val name = safeFieldName(v, k, 0, seen)
                  acc.name(name).`type`(v).noDefault()
              }
              .endRecord()
            includeProps(avro, props)
            avro.addProp(RECORD_TYPE_KEY, "obj")
            (registry + (fingerprint -> avro), avro)
          }
        }

      case RowF(elements, props) =>
        val fingerprint = elements.hashCode()
        State { registry =>
          if (registry.contains(fingerprint)) {
            (registry, registry(fingerprint))
          } else {
            val seen = mutable.Map.empty[String, Int].withDefaultValue(0)
            val avro = elements.zipWithIndex
              .foldLeft(SchemaBuilder.record("r%x".format(fingerprint)).fields()) {
                case (acc, (col, idx)) =>
                  val name = safeFieldName(col.value, col.header.getOrElse(""), idx, seen)
                  if (col.header.isEmpty) {
                    col.value.addProp(NO_HEADER, true)
                  }
                  acc.name(name).`type`(col.value).noDefault()
              }
              .endRecord()
            includeProps(avro, props)
            avro.addProp(RECORD_TYPE_KEY, "row")
            (registry + (fingerprint -> avro), avro)
          }
        }

      case todo =>
        assert(false, s"TODO: Convert ${pprint.apply(todo)}")
        ???
    }
  }

  val coalgebra: Coalgebra[SchemaF, AvroSchema] = Coalgebra[SchemaF, AvroSchema] {

    // These are internal properties that should be filtered out
    val filter = Set(
      RECORD_TYPE_KEY,
      ORIGINAL_NAME_KEY,
      NO_HEADER
    )

    def extractProps(avro: AvroSchema) = {
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
      field.schema().getProp(ORIGINAL_NAME_KEY) match {
        case null => field.name()
        case orig => orig
      }
    }

    avro =>
      avro.getType match {
        case AvroSchema.Type.INT     => ValueF(IntType, extractProps(avro))
        case AvroSchema.Type.LONG    => ValueF(LongType, extractProps(avro))
        case AvroSchema.Type.FLOAT   => ValueF(FloatType, extractProps(avro))
        case AvroSchema.Type.DOUBLE  => ValueF(DoubleType, extractProps(avro))
        case AvroSchema.Type.BOOLEAN => ValueF(BooleanType, extractProps(avro))
        case AvroSchema.Type.STRING  => ValueF(TextType, extractProps(avro))
        case AvroSchema.Type.BYTES   => ValueF(BytesType, extractProps(avro))

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
                val noHeader = field.schema().getObjectProp(NO_HEADER).asInstanceOf[Boolean]

                val header =
                  if (noHeader) None
                  else Some(originalName(field))

                Column(field.schema(), header)
              }.toVector

              RowF(columns, extractProps(avro))

            case _ => ???
          }
        case todo => ???
      }
  }

  private val toAvroSchemaFn = scheme.cataM(algebra)

  private val fromAvroSchemaFn = scheme.ana(coalgebra)

  def toAvroSchema(schema: Schema): AvroSchema = {
    toAvroSchemaFn(schema).run(Map.empty).value._2
  }

  def fromAvroSchema(schema: AvroSchema): Schema = {
    fromAvroSchemaFn(schema)
  }
}
