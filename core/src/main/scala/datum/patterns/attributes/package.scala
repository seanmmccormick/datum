package datum.patterns

import qq.droste.data.Fix
import qq.droste.syntax.fix._

package object attributes {
  type Attribute = Fix[AttributesF]
  type AttributeMap = Map[String, Attribute]

  val flag: Attribute = Flag.fix

  def property(bool: Boolean): Attribute = BoolProperty(bool).fix

  def property(text: String): Attribute = Property(text).fix

  def property(value: Double): Attribute = NumProperty(value).fix

  def label(name: String, attr: Attribute): Attribute = Label(name, attr).fix[AttributesF]

  def collection(values: Attribute*): Attribute = Collection(values.toVector).fix[AttributesF]

}
