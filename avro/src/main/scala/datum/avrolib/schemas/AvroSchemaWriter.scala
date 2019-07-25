package datum.avrolib.schemas
import cats.data.State
import datum.patterns.schemas._
import datum.avrolib.properties._
import higherkindness.droste.{AlgebraM, scheme}
import org.apache.avro.{LogicalType, LogicalTypes, SchemaBuilder, Schema => AvroSchema}

import scala.collection.mutable
import collection.JavaConverters._

object AvroSchemaWriter {

  def includeProps(avro: AvroSchema, props: PropertyMap): AvroSchema = {
    assert(!avro.isUnion, "Tried to includeProps on a union!")
    props.foreach {
      case (k, prop) =>
        avro.addProp(k, prop.toAvro)
    }
    avro
  }

  val algebra: AlgebraM[Registry, SchemaF, AvroSchema] = AlgebraM {
    case ValueF(IntType, props)     => primitive(AvroSchema.Type.INT, props)
    case ValueF(LongType, props)    => primitive(AvroSchema.Type.LONG, props)
    case ValueF(FloatType, props)   => primitive(AvroSchema.Type.FLOAT, props)
    case ValueF(DoubleType, props)  => primitive(AvroSchema.Type.DOUBLE, props)
    case ValueF(BooleanType, props) => primitive(AvroSchema.Type.BOOLEAN, props)
    case ValueF(TextType, props)    => primitive(AvroSchema.Type.STRING, props)
    case ValueF(BytesType, props)   => primitive(AvroSchema.Type.BYTES, props)

    case ValueF(DateType, props) =>
      primitive(AvroSchema.Type.STRING, props).map { avro =>
        avro.addProp(RECORD_TYPE_KEY, "date")
        avro
      }

    case ValueF(TimestampType, props) => logical(AvroSchema.Type.LONG, LogicalTypes.timestampMillis(), props)

    case ValueF(DateTimeType, props) =>
      primitive(AvroSchema.Type.STRING, props).map { avro =>
        avro.addProp(RECORD_TYPE_KEY, "date-time")
        avro
      }

    case ValueF(ZonedDateTimeType, props) =>
      primitive(AvroSchema.Type.STRING, props).map { avro =>
        avro.addProp(RECORD_TYPE_KEY, "zoned-date-time")
        avro
      }

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
                val (name, modified) = uniqueName(k, 0, seen)
                if (modified) {
                  safeAddProp(v, ORIGINAL_NAME_KEY, k)
                }
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
                val original = col.header.getOrElse("")
                val (name, modified) = uniqueName(original, idx, seen)
                if (modified) {
                  safeAddProp(col.value, ORIGINAL_NAME_KEY, original)
                }
                if (col.header.isEmpty) {
                  safeAddProp(col.value, NO_HEADER, true)
                }
                acc.name(name).`type`(col.value).noDefault()
            }
            .endRecord()
          includeProps(avro, props)
          avro.addProp(RECORD_TYPE_KEY, "row")
          (registry + (fingerprint -> avro), avro)
        }
      }

    case ArrayF(conforms, props) =>
      State.pure {
        val avro = AvroSchema.createArray(conforms)
        includeProps(avro, props)
        avro
      }

    case NamedUnionF(alts, props) =>
      val fingerprint = alts.hashCode()
      union(fingerprint, props, "named") { buffer =>
        val seen = mutable.Map.empty[String, Int].withDefaultValue(0)
        alts.foreach {
          case (k, v) =>
            val (name, modified) = uniqueName(k, 0, seen)
            val record = SchemaBuilder.record(name).fields().name("schema").`type`(v).noDefault().endRecord()
            if (modified) {
              record.addProp(ORIGINAL_NAME_KEY, k)
            }
            buffer.append(record)
        }
      }

    case IndexedUnionF(alts, props) =>
      val fingerprint = alts.hashCode()
      union(fingerprint, props, "indexed") { buffer =>
        alts.zipWithIndex.foreach {
          case (v, idx) =>
            buffer.append(SchemaBuilder.record(s"_$idx").fields().name("schema").`type`(v).noDefault().endRecord())
        }
      }
  }

  private def primitive(typ: AvroSchema.Type, props: PropertyMap): Registry[AvroSchema] = {
    State.pure {
      val result = AvroSchema.create(typ)
      includeProps(result, props)
    }
  }

  private def logical(typ: AvroSchema.Type, logical: LogicalType, props: PropertyMap): Registry[AvroSchema] = {
    State.pure {
      val result = AvroSchema.create(typ)
      logical.addToSchema(result)
      includeProps(result, props)
    }
  }

  private def union(
    fingerprint: Int,
    props: PropertyMap,
    `type`: String
  )(fillFn: mutable.Buffer[AvroSchema] => Unit): Registry[AvroSchema] = {
    State { registry =>
      if (registry.contains(fingerprint)) {
        (registry, registry(fingerprint))
      } else {
        val buffer = mutable.Buffer.empty[AvroSchema]
        fillFn(buffer)

        // union types can't call addProp directly, so append a fake record to store those props
        val workaround = SchemaBuilder.record("$datum.properties_r%x".format(fingerprint)).fields().endRecord()
        includeProps(workaround, props)
        workaround.addProp(RECORD_TYPE_KEY, `type`)
        buffer.append(workaround)
        val avro = AvroSchema.createUnion(buffer.asJava)
        (registry + (fingerprint -> avro), avro)
      }
    }
  }

  // Helper function to ensure only valid (no special characters, no duplicates, etc..) names
  private def safeName(inp: String): (String, Boolean) = {
    val builder = mutable.StringBuilder.newBuilder
    var modified = false

    inp.iterator.foreach {
      case c if c.isLetterOrDigit =>
        builder.append(c)

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
  private def safeFieldNamez(field: AvroSchema, original: String, idx: Int, seen: mutable.Map[String, Int]): String = {

    def next(check: String): String = {
      val result =
        if (seen(check) == 0) check
        else next(s"$check${seen(check) + 1}")
      seen(check) += 1
      result
    }

    val (safe, modified) = safeName(original)

    if (modified) {
      safeAddProp(field, ORIGINAL_NAME_KEY, original)
    }

    if (seen(safe) == 0) {
      seen(safe) += 1
      safe
    } else {
      if (!modified) {
        safeAddProp(field, ORIGINAL_NAME_KEY, original)
      }
      next(safe)
    }
  }

  private def uniqueName(original: String, idx: Int, seen: mutable.Map[String, Int]): (String, Boolean) = {
    def next(check: String): String = {
      val result =
        if (seen(check) == 0) check
        else next(s"$check${seen(check) + 1}")
      seen(check) += 1
      result
    }

    val (safe, modified) = safeName(original)

    if (seen(safe) == 0) {
      seen(safe) += 1
      (safe, modified)
    } else {
      (next(safe), true)
    }
  }

  // This uses the convention that a fake record is appended to the end of a union to carry
  // (datum) properties around
  private def safeAddProp(field: AvroSchema, key: String, prop: Any): AvroSchema = {
    if (field.isUnion) {
      field.getTypes.asScala.last.addProp(key, prop)
    } else {
      field.addProp(key, prop)
    }
    field
  }

  private val toAvroSchemaFn = scheme.cataM(algebra)

  def write(schema: Schema): AvroSchema = {
    toAvroSchemaFn(schema).run(Map.empty).value._2
  }

  def using(algebra: AlgebraM[Registry, SchemaF, AvroSchema]): Schema => AvroSchema = {
    val fn = scheme.cataM(algebra)
    schema =>
      fn(schema).run(Map.empty).value._2
  }

}
