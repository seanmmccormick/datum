package datum.blue.attributes

trait AttributeValue extends Product with Serializable

// Generic Values
final case class Flag(value: Boolean) extends AttributeValue
final case class Property(value: String) extends AttributeValue