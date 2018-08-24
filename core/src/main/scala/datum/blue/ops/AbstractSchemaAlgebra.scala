package datum.blue.ops

import cats.data.State
import datum.blue.attributes.{Attributed, Attributes}
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

  import cats.implicits._

  def toFunction2[In, Out](fold: Folder[In => Out])(
    modifying: Attributed[Out] => Attributed[Out]): Algebra[SchemaF, In => Out] = {

    def eval(attrs: Attributes)(fold: In => Out): In => Out = {
      (modifying <<< State.pure <<< fold) >>> { _.runA(attrs).value }
    }

    {
      case ValueF(BooleanType, attrs) => eval(attrs)(fold.onBoolean)
      case ValueF(IntegerType, attrs) => eval(attrs)(fold.onInteger)
      case ValueF(TextType, attrs)    => eval(attrs)(fold.onText)
      case ValueF(RealType, attrs)    => eval(attrs)(fold.onReal)
      case StructF(fields, attrs)     => eval(attrs)(fold.onStruct(fields))
    }
  }
}
