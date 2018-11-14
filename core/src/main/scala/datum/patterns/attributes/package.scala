package datum.patterns

import cats.data.State
import qq.droste.data.Fix

package object attributes {
  type Attribute = Fix[AttributesF]

  type AttributeMap = Map[AttributeKey, Attribute]

  type Attributed[T] = State[AttributeMap, T]

  implicit class AttributeStringOps(inp: String) {
    def asAttributeKey: AttributeKey = AttributeKey(inp)
  }

  def property(flag: Boolean): Attribute = Fix.apply[AttributesF](BooleanPropertyF(flag))

  def property(text: String): Attribute = Fix.apply[AttributesF](TextPropertyF(text))

  def and(left: Attribute, right: Attribute): Attribute = Fix.apply[AttributesF](AndF(left, right))

  def or(left: Attribute, right: Attribute): Attribute = Fix.apply[AttributesF](OrF(left, right))

  def label(name: String, attr: Attribute): Attribute = Fix.apply[AttributesF](LabelF(name, attr))
}
