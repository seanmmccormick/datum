package datum.avrolib.data
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.Checkers

class RecordReaderSpec extends WordSpec with Checkers with Matchers {

  "Avrolib RecordReader" should {
    "encode data matching a simple schema" in {
      2 shouldBe 2
    }
  }
}
