package datum.blue.ops

import cats.Monad
import datum.blue.attributes.Attributes
import datum.blue.schema._
import turtles.{Algebra, AlgebraM}

import scala.collection.immutable.SortedMap

object AbstractSchemaAlgebra {

  trait Folder[X] {
    def onBoolean: X
    def onText: X
    def onReal: X
    def onInteger: X
    def onStruct(fields: SortedMap[String, X]): X
  }

  def toFunction[In, Out](fold: Folder[In => Out])(
    check: PartialFunction[(Out, Attributes), Out]
  ): Algebra[SchemaF, In => Out] = {

    def checking(out: Out, attrs: Attributes): Out = {
      check.applyOrElse[(Out, Attributes), Out]((out, attrs), _ => out)
    }

    {
      case ValueF(BooleanType, attrs) =>
        in =>
          checking(fold.onBoolean(in), attrs)

      case ValueF(IntegerType, attrs) =>
        in =>
          checking(fold.onInteger(in), attrs)

      case ValueF(TextType, attrs) =>
        in =>
          checking(fold.onText(in), attrs)

      case ValueF(RealType, attrs) =>
        in =>
          checking(fold.onReal(in), attrs)

      case StructF(fields, attrs) =>
        in =>
          checking(fold.onStruct(fields)(in), attrs)
    }
  }
}
