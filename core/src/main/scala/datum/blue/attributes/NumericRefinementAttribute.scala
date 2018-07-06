package datum.blue.attributes

sealed trait NumericRefinementAttribute extends AttributeValue
case object Positive extends NumericRefinementAttribute
final case class LessThan(max: Double) extends NumericRefinementAttribute
final case class GreaterThan(min: Double) extends NumericRefinementAttribute
final case class UnitOfMeasure(label: String) extends NumericRefinementAttribute
