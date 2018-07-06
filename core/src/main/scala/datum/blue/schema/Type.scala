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