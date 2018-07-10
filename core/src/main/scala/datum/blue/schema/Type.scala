package datum.blue.schema

sealed trait Type
case object IntegerType extends Type
case object RealType extends Type
case object TextType extends Type
case object ByteType extends Type
case object BooleanType extends Type
case object DateType extends Type
case object InstantType extends Type
case object LocalTimeType extends Type
case object ZonedTimeType extends Type

object Type {

  def asString(inp: Type): String = inp match {
    case IntegerType => "integer"
    case RealType => "real"
    case TextType => "text"
    case ByteType => "byte"
    case BooleanType => "boolean"
    case DateType => "date"
    case InstantType => "instant"
    case LocalTimeType => "local_time"
    case ZonedTimeType => "zoned_time"
  }
}