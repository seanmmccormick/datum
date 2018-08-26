package datum.green.foo

import datum.green.SchemaGen
import org.scalacheck.Arbitrary
import org.scalatest.{Matchers, WordSpec}

class FooSpec extends WordSpec with Matchers {

  "foo" should {
    "foo" in {
      println("Wat?")
      val x = SchemaGen.genStruct().sample
      pprint.pprintln(x)
    }
  }

}
