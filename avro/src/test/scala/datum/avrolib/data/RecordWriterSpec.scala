package datum.avrolib.data

import datum.modifiers.Optional
import datum.patterns.{data => d}
import datum.patterns.schemas
import datum.patterns.schemas._

import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.Checkers

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
  }
}
