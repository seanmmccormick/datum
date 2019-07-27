package datum.avrolib.util
import datum.avrolib.data.{RecordReader, RecordWriter}
import datum.avrolib.schemas.AvroSchemaWriter
import datum.patterns.data.Data
import datum.patterns.schemas.Schema
import org.apache.avro.file.{DataFileReader, DataFileWriter}
import org.apache.avro.generic.{GenericDatumReader, GenericDatumWriter, GenericRecord}
import org.apache.avro.generic.GenericData.{Record => AvroRecord}

import scala.collection.mutable

object Roundtrip {

  def apply(schema: Schema, data: List[Data]): List[Data] = {
    val avro = AvroSchemaWriter.write(schema)
    assert(!avro.isUnion, "Top level schema can't be a union (ie optional)")

    val writer = new RecordWriter(AvroSchemaWriter.write)
    val toGenericRecord = writer.define(writer.optional(writer.algebra))(schema)
    val fromGenericRecord = RecordReader.generateFor(schema)

    val temp = java.io.File.createTempFile("roundtrip", ".avro")

    try {
      // to write a file
      val writer = new DataFileWriter[GenericRecord](new GenericDatumWriter[GenericRecord]()).create(avro, temp)
      data.map(toGenericRecord).foreach(writer.append)
      writer.flush()
      writer.close()

      // to read a file
      val reader = new DataFileReader[GenericRecord](temp, new GenericDatumReader[GenericRecord]())
      val record = new AvroRecord(reader.getSchema)
      val buffer = mutable.ListBuffer.empty[Data]
      while (reader.hasNext) {
        reader.next(record)
        buffer.append(fromGenericRecord(record))
      }
      buffer.toList
    } catch {
      case e: Exception =>
        throw e
    } finally {
      temp.delete()
    }
  }
}
