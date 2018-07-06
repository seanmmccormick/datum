package datum.blue

import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.WordSpec

class MySuite extends WordSpec with Checkers {

  "any list" should {
    "size law" in {
      check((a: List[Int], b: List[Int]) => a.size + b.size == (a ::: b).size)
    }
  }
}
