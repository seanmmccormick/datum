package datum.blue.schema

sealed trait Refinement

sealed trait TextRefinement
case class Regex(pattern: String) extends TextRefinement

sealed trait IntRefinement
case object PositiveInt extends IntRefinement
