package datum.green.patterns.schemas

sealed trait Type
case object IntType extends Type
case object LongType extends Type
case object FloatType extends Type
case object DoubleType extends Type
case object TextType extends Type
case object BytesType extends Type
case object BooleanType extends Type
case object DateType extends Type
case object InstantType extends Type
case object LocalTimeType extends Type
case object ZonedTimeType extends Type

object Type {

  def asString(inp: Type): String = inp match {
    case IntType       => "int"
    case LongType      => "long"
    case FloatType     => "float"
    case DoubleType    => "double"
    case TextType      => "text"
    case BytesType     => "bytes"
    case BooleanType   => "boolean"
    case DateType      => "date"
    case InstantType   => "instant"
    case LocalTimeType => "local_time"
    case ZonedTimeType => "zoned_time"
  }

  def fromString(inp: String): Option[Type] = inp match {
    case "int"        => Some(IntType)
    case "long"       => Some(LongType)
    case "float"      => Some(FloatType)
    case "double"     => Some(DoubleType)
    case "text"       => Some(TextType)
    case "bytes"      => Some(BytesType)
    case "boolean"    => Some(BooleanType)
    case "date"       => Some(DateType)
    case "instant"    => Some(InstantType)
    case "local_time" => Some(LocalTimeType)
    case "zoned_time" => Some(ZonedTimeType)
    case _            => None
  }
}
