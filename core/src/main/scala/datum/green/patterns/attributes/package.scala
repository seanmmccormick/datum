package datum.green.patterns

import cats.data.State
import qq.droste.data.Fix

package object attributes {
  type Attribute = Fix[AttributesF]

  type Attributed[T] = State[Map[AttributeKey, Attribute], T]

  implicit class AttributeStringOps(inp: String) {
    def asAttributeKey: AttributeKey = AttributeKey(inp)
  }
}
