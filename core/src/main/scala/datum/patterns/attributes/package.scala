package datum.patterns

import qq.droste.data.Fix

package object attributes {
  type Attribute = Fix[AttributesF]
  type AttributeMap = Map[String, Attribute]

  def property(flag: Boolean): Attribute = Fix.apply[AttributesF](BoolProperty(flag))

  def property(text: String): Attribute = Fix.apply[AttributesF](Property(text))

  def property(value: Double): Attribute = Fix.apply[AttributesF](NumProperty(value))

  def and(left: Attribute, right: Attribute): Attribute = Fix.apply[AttributesF](And(left, right))

  def or(left: Attribute, right: Attribute): Attribute = Fix.apply[AttributesF](Or(left, right))

  def label(name: String, attr: Attribute): Attribute = Fix.apply[AttributesF](Label(name, attr))

  def collection(values: Attribute*): Attribute = Fix.apply[AttributesF](Collection(values.toVector))
}
