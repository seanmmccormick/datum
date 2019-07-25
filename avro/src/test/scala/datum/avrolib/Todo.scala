package datum.avrolib

import datum.avrolib.data.{RecordReader, RecordWriter}
import datum.avrolib.schemas.{AvroSchemaWriter, AvroSchemaReader}
import datum.gen.algebras.DataGen
import datum.modifiers.Optional
import datum.patterns.data.Data
import datum.patterns.{schemas => s}
import datum.patterns.schemas._
import org.apache.avro.file.{CodecFactory, DataFileReader, DataFileWriter}
import org.apache.avro.generic.GenericData.{Record => AvroRecord}
import org.apache.avro.generic.{GenericDatumReader, GenericDatumWriter, GenericRecord}
import org.scalacheck.rng.Seed
import org.scalacheck.Gen

import scala.collection.mutable

object Todo extends App {

//  val simple = s.obj()(
//    "foo" -> s.value(IntType),
//    "Unnamed" -> s.value(TextType),
//    "" -> s.obj()(
//      "foo" -> s.value(IntType),
//      "bar" -> s.value(BooleanType, Optional.enable)
//    ),
//    "bar" -> s.value(BooleanType, Optional.enable)
//  )

  val simple = s.row()(
    s.col("foo", s.value(IntType)),
    s.col("",
          s.obj()(
            "foo" -> s.value(IntType),
            "bar" -> s.value(BooleanType, Optional.enable)
          )),
    s.col("Unnamed1", s.value(TextType)),
    s.col("bar", s.value(BooleanType))
  )

  val toGenericRecord = RecordWriter.generateFor(simple)

  implicit val zzz = DataGen.define().apply(simple)

  val wat: List[Data] = (0 until 1000).map(i => zzz.pureApply(Gen.Parameters.default, Seed(i))).toList

  pprint.pprintln(wat.map(toGenericRecord))

  // to write a file:
  val avro = AvroSchemaWriter.write(simple)

  val hrm = new GenericDatumWriter[GenericRecord]()

  val derp = new java.io.File("testing.avro")

  derp.createNewFile()

  val foo = new DataFileWriter[GenericRecord](hrm).setCodec(CodecFactory.bzip2Codec()).create(avro, derp)

  wat.map(toGenericRecord).foreach(foo.append)

  foo.close()

  // to read a file
  val gdr = new GenericDatumReader[GenericRecord]()

  val reader = new DataFileReader[GenericRecord](derp, gdr)

  val record = new AvroRecord(reader.getSchema)

  val buffer = mutable.ListBuffer.empty[Data]

  val fromGenericRecord = RecordReader.generateFor(simple)

  println("===============================================")
  while (reader.hasNext) {
    reader.next(record)
    pprint.pprintln(record)
    pprint.pprintln(fromGenericRecord(record))
    buffer.append(fromGenericRecord(record))
  }

  println(buffer.toList == wat)

}
