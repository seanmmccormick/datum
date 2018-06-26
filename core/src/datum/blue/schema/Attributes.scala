package datum.blue.schema

class Attributes {

}

sealed trait AttributeKey

sealed trait WellKnownKey extends AttributeKey
case object Optional extends WellKnownKey
case class Extension(key: String) extends AttributeKey

sealed trait AttributeValue
case class Flag(value: Boolean) extends AttributeValue
case class Property(value: String) extends AttributeValue