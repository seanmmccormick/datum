package datum.algebras.generic

import cats.data.State
import cats.implicits._
import datum.patterns.attributes.{Attribute, AttributeKey, Modifiable}
import datum.patterns.schemas._
import qq.droste._

trait ModifiableFunction[In, Out] {

  def modify: Modifiable[Out] => Modifiable[Out]

  def base: Algebra[SchemaF, In => Out]

  private def run(attrs: Map[AttributeKey, Attribute])(initial: In => Out): In => Out = {
    (modify <<< State.pure <<< initial) >>> { _.runA(attrs).value }
  }

  def algebra: Algebra[SchemaF, In => Out] = Algebra {
    case x @ ValueF(_, attrs) => run(attrs)(base(x))
    case x @ ObjF(_, attrs)   => run(attrs)(base(x))
    case x @ RowF(_, attrs)   => run(attrs)(base(x))
    case x @ ArrayF(_, attrs) => run(attrs)(base(x))
    case x @ UnionF(_, attrs) => run(attrs)(base(x))
  }

  private val generator = scheme.cata(algebra)

  def generateFor(schema: Schema): In => Out = {
    generator(schema)
  }
}
