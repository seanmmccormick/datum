package datum.patterns

import java.time._

import higherkindness.droste.data.Fix

import scala.collection.immutable.SortedMap

package object data {
  type Data = Fix[DataF]

  implicit class DataOps(val data: Data) extends AnyVal {
    def project: DataF[Data] = Fix.un[DataF](data)
  }

  def obj(fields: (String, Data)*): Data = {
    Fix(ObjValue(SortedMap(fields: _*)))
  }

  def obj(fields: SortedMap[String, Data]): Data = {
    Fix(ObjValue(fields))
  }

  def row(
    columns: Vector[Data]
  ): Data = {
    Fix(RowValue(columns))
  }

  def row(
    columns: Data*
  ): Data = {
    Fix(RowValue(Vector(columns: _*)))
  }

  def array(elements: Vector[Data]): Data = row(elements)

  def array(elements: Data*): Data = row(elements: _*)

  def union(selection: String, element: Data): Data = {
    Fix(NamedUnionValue(selection, element))
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

  def timestamp(value: Instant): Data = {
    Fix[DataF](TimestampValue(value))
  }

  def localTime(value: LocalDateTime): Data = {
    Fix[DataF](LocalDateTimeValue(value))
  }

  def zonedTime(value: ZonedDateTime): Data = {
    Fix[DataF](ZonedDateTimeValue(value))
  }

  def bytes(value: Array[Byte]): Data = {
    Fix[DataF](BytesValue(value))
  }

  val empty: Data = {
    Fix[DataF](EmptyValue)
  }

  def typeOf(record: Data): String = Fix.un[DataF](record) match {
    case ObjValue(_)             => "obj"
    case RowValue(_)             => "row"
    case NamedUnionValue(_, _)   => "named-union"
    case IndexedUnionValue(_, _) => "indexed-union"
    case IntValue(_)             => "int"
    case LongValue(_)            => "long"
    case FloatValue(_)           => "float"
    case DoubleValue(_)          => "double"
    case TextValue(_)            => "text"
    case BooleanValue(_)         => "boolean"
    case BytesValue(_)           => "bytes"
    case DateValue(_)            => "date"
    case TimestampValue(_)       => "timestamp"
    case LocalDateTimeValue(_)   => "local-date-time"
    case ZonedDateTimeValue(_)   => "zoned-date-time"
    case EmptyValue              => "empty"
  }
}
