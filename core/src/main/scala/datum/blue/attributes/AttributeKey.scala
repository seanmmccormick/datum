package datum.blue.attributes

sealed trait AttributeKey extends Product with Serializable

final case class Key(value: String) extends AttributeKey

sealed trait WellKnownKey extends AttributeKey {
  type ValueType
}

case object Optional extends WellKnownKey {
  override type ValueType = Flag
}

case object TextRefinement extends WellKnownKey {
  override type ValueType = TextRefinementAttribute
}

case object NumericRefinement extends WellKnownKey {
  override type ValueType = NumericRefinementAttribute
}