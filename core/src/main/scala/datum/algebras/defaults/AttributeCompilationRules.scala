package datum.algebras.defaults
import datum.patterns.attributes._
import datum.patterns.data
import datum.patterns.data.Data

import qq.droste.data.Fix
import cats.syntax.either._

trait CompileBoolean {

  def boolean(attribute: Attribute): Either[String, Data] = Fix.un[AttributesF](attribute) match {
    case BooleanPropertyF(value) => Right(data.boolean(value))
    case _                       => Left("Todo: Boolean Error")
  }
}

trait CompileInteger {

  def integer(attribute: Attribute): Either[String, Data] = Fix.un[AttributesF](attribute) match {
    case NumericPropertyF(value) => Either.catchNonFatal(data.integer(value.toInt)).leftMap(_.getMessage)
    case _                       => Left("Todo: Numeric Error")
  }
}

trait CompileText {

  def text(attribute: Attribute): Either[String, Data] = Fix.un[AttributesF](attribute) match {
    case TextPropertyF(value) => Right(data.text(value))
    case _                    => Left("Todo: Text Value Error")
  }
}

trait AttributeCompilationRules {
  def boolean(attribute: Attribute): Either[String, Data]
  def integer(attribute: Attribute): Either[String, Data]
  def text(attribute: Attribute): Either[String, Data]
}

// Alternative rules for turning Attributes into Data can be supplied by creating another
// Rules object with custom overrides
object DefaultCompilationRules
  extends AttributeCompilationRules
  with CompileBoolean
  with CompileInteger
  with CompileText
