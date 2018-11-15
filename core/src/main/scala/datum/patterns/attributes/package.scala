package datum.patterns

import cats.data.{Reader, State}
import qq.droste.data.Fix

package object attributes {
  type Attribute = Fix[AttributesF]

  type AttributeMap = Map[AttributeKey, Attribute]

  type Attributed[T] = Reader[AttributeMap, T]
  type Modifiable[T] = State[AttributeMap, T]

  def default[A]: A => Attributed[A] = cats.data.ReaderT.pure

  implicit class AttributeStringOps(inp: String) {
    def asAttributeKey: AttributeKey = AttributeKey(inp)
  }

  def property(flag: Boolean): Attribute = Fix.apply[AttributesF](BooleanPropertyF(flag))

  def property(text: String): Attribute = Fix.apply[AttributesF](TextPropertyF(text))

  def and(left: Attribute, right: Attribute): Attribute = Fix.apply[AttributesF](AndF(left, right))

  def or(left: Attribute, right: Attribute): Attribute = Fix.apply[AttributesF](OrF(left, right))

  def label(name: String, attr: Attribute): Attribute = Fix.apply[AttributesF](LabelF(name, attr))
}
