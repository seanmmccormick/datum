package datum

import dataum.data._
import schemes.Fix

import scala.collection.immutable.SortedMap

package object data {

  type Data = Fix[DataF]

  def struct(fields: SortedMap[String, Data]): Data = Fix.apply[DataF](StructValue(fields))

  def text(value: String): Data = Fix.apply[DataF](TextValue(value))

  def int(value: Int): Data = Fix.apply[DataF](IntValue(value))
}
