package datum.blue

import turtles.{Birecursive, Corecursive, Recursive}
import turtles.data.Fix

package object meta {

  type Meta = Fix[MetaF]

  type MetaMap = Map[AttrKey, Meta]

  def property(text: String): Meta = Birecursive[Meta].embed(TextPropertyF(text))

  def property(number: Double): Meta = Birecursive[Meta].embed(NumericPropertyF(number))

  def property(flag: Boolean): Meta = Birecursive[Meta].embed(BooleanPropertyF(flag))

  def label(name: String, value: Meta): Meta = Birecursive[Meta].embed(LabelF(name, value))

  def isOptional(attrs: MetaMap): Boolean = {
    val Flag = property(true)
    attrs.collectFirst {
      case (common.optional, Flag) => true
    }.isDefined
  }
}
