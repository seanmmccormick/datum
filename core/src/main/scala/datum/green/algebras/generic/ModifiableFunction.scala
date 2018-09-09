package datum.green.algebras.generic

import cats.data.State
import cats.implicits._
import datum.green.patterns.attributes.{Attribute, AttributeKey, Attributed}
import datum.green.patterns.schemas._
import qq.droste._

trait ModifiableFunction[In, Out] {

  def modify: Attributed[Out] => Attributed[Out]

  def base: Algebra[SchemaF, In => Out]

  private def run(attrs: Map[AttributeKey, Attribute])(initial: In => Out): In => Out = {
    (modify <<< State.pure <<< initial) >>> { _.runA(attrs).value }
  }

  def algebra: Algebra[SchemaF, In => Out] = Algebra {
    case x @ ValueF(_, attrs)  => run(attrs)(base(x))
    case x @ StructF(_, attrs) => run(attrs)(base(x))
    case x @ RowF(_, attrs)    => run(attrs)(base(x))
    case x @ ArrayF(_, attrs)  => run(attrs)(base(x))
    case x @ UnionF(_, attrs)  => run(attrs)(base(x))
  }

  private val generator = scheme.cata(algebra)

  def generateFor(schema: Schema): In => Out = {
    generator(schema)
  }
}
