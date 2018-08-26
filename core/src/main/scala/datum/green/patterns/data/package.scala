package datum.green.patterns

import qq.droste.data.Fix

import scala.collection.immutable.SortedMap

package object data {
  type Data = Fix[DataF]

  def struct(fields: (String, Data)*): Data = {
    Fix(StructDataF(SortedMap(fields: _*)))
  }

  def row(
    elements: Vector[Data]
  ): Data = {
    Fix(RowDataF(elements))
  }

  def text(value: String): Data = {
    Fix[DataF](TextDataF(value))
  }

  def real(value: Double): Data = {
    Fix[DataF](RealDataF(value))
  }

  def integer(value: Long): Data = {
    Fix[DataF](IntegerDataF(value))
  }

  def boolean(value: Boolean): Data = {
    Fix[DataF](BooleanDataF(value))
  }

  def empty: Data = {
    Fix[DataF](EmptyDataF)
  }
}
