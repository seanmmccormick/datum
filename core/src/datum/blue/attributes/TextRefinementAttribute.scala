package datum.blue.attributes

sealed trait TextRefinementAttribute extends AttributeValue
final case class RegexRefinement(expr: String) extends TextRefinementAttribute