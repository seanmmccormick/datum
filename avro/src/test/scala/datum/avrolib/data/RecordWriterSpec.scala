package datum.avrolib.data

import datum.avrolib.schemas.AvroSchemaWriter
import datum.gen.algebras.DataGen
import datum.modifiers.Optional
import datum.patterns.data.Data
import datum.patterns.{data => d}
import datum.patterns.schemas
import datum.patterns.schemas._
import org.apache.avro.file.{DataFileReader, DataFileWriter}
import org.apache.avro.generic.{GenericDatumReader, GenericDatumWriter, GenericRecord}
import org.apache.avro.generic.GenericData.{Record => AvroRecord}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Prop._
import org.scalatestplus.scalacheck.Checkers

import scala.collection.mutable

class RecordWriterSpec extends WordSpec with Checkers with Matchers {

  "Avrolib RecordWriter" should {
    "encode data matching a simple schema" in {
      val simple = schemas.obj()(
        "foo" -> schemas.value(IntType),
        "bar" -> schemas.value(BooleanType, Optional.enable)
      )
      val r1 = d.obj("foo" -> d.integer(10), "bar" -> d.boolean(true))
      val toGenericRecord = RecordWriter.generateFor(simple)
      val generic = toGenericRecord(r1)

      generic.get("foo") shouldBe 10
      generic.get("bar") shouldBe true
    }

    "encode data for an indexed union" in {
      val schema = schemas.obj()(
        "test" -> schemas.indexed()(schemas.value(IntType), schemas.value(IntType)),
        "other" -> schemas.union()("a" -> schemas.value(BooleanType), "" -> schemas.value(TextType))
      )

      implicit val arb: Arbitrary[List[Data]] = Arbitrary {
        for {
          data <- Gen.nonEmptyListOf(DataGen.define()(schema))
        } yield data
      }

      check {
        forAll { data: List[Data] =>
          Roundtrip(schema, data) == data
        }
      }
    }
  }
}

object Roundtrip {

  def apply(schema: Schema, data: List[Data]): List[Data] = {
    val avro = AvroSchemaWriter.write(schema)
    val toGenericRecord = RecordWriter.generateFor(schema)

    val hrm = new GenericDatumWriter[GenericRecord]()
    val derp = new java.io.File("testing.avro")
    derp.createNewFile()
    val foo = new DataFileWriter[GenericRecord](hrm).create(avro, derp)

    data.map(toGenericRecord).foreach(foo.append)

    foo.close()

    // to read a file
    val gdr = new GenericDatumReader[GenericRecord]()

    val reader = new DataFileReader[GenericRecord](derp, gdr)

    val record = new AvroRecord(reader.getSchema)

    val buffer = mutable.ListBuffer.empty[Data]

    val fromGenericRecord = RecordReader.generateFor(schema)

    while (reader.hasNext) {
      reader.next(record)
      buffer.append(fromGenericRecord(record))
    }
    buffer.toList
  }
}
