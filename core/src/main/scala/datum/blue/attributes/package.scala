package datum.blue

import cats.data.State
import datum.blue.schema.SchemaF
import turtles.{Birecursive, Corecursive, Recursive}
import turtles.data.Fix

package object attributes {

  type Attr = Fix[AttrF]

  type Attributes = Map[AttrKey, Attr]

  def property(text: String): Attr = Birecursive[Attr].embed(TextPropertyF(text))

  def property(number: Double): Attr = Birecursive[Attr].embed(NumericPropertyF(number))

  def property(flag: Boolean): Attr = Birecursive[Attr].embed(BooleanPropertyF(flag))

  def label(name: String, value: Attr): Attr = Birecursive[Attr].embed(LabelF(name, value))

  def isOptional(attrs: Attributes): Boolean = {
    val Flag = property(true)
    attrs.collectFirst {
      case (common.optional, Flag) => true
    }.isDefined
  }

  type Attributed[X] = State[Attributes, X]


}
