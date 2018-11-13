package datum.patterns

import java.time._

import qq.droste.data.Fix

import scala.collection.immutable.SortedMap

package object data {
  type Data = Fix[DataF]

  def struct(fields: (String, Data)*): Data = {
    Fix(ObjValue(SortedMap(fields: _*)))
  }

  def struct(fields: SortedMap[String, Data]): Data = {
    Fix(ObjValue(fields))
  }
  
  def row(
    elements: Vector[Data]
  ): Data = {
    Fix(RowValue(elements))
  }

  def text(value: String): Data = {
    Fix[DataF](TextValue(value))
  }

  def double(value: Double): Data = {
    Fix[DataF](DoubleValue(value))
  }

  def float(value: Float): Data = {
    Fix[DataF](FloatValue(value))
  }

  def integer(value: Int): Data = {
    Fix[DataF](IntValue(value))
  }

  def long(value: Long): Data = {
    Fix[DataF](LongValue(value))
  }

  def boolean(value: Boolean): Data = {
    Fix[DataF](BooleanValue(value))
  }

  def date(value: LocalDate): Data = {
    Fix[DataF](DateValue(value))
  }

  def instant(value: Instant): Data = {
    Fix[DataF](InstantValue(value))
  }

  def localTime(value: LocalDateTime): Data = {
    Fix[DataF](LocalTimeValue(value))
  }

  def zonedTime(value: ZonedDateTime): Data = {
    Fix[DataF](ZonedTimeValue(value))
  }

  def bytes(value: Array[Byte]): Data = {
    Fix[DataF](BytesValue(value))
  }

  def empty: Data = {
    Fix[DataF](EmptyValue)
  }
}
