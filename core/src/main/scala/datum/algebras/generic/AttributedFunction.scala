package datum.algebras.generic
import datum.patterns.attributes.Attributed
import datum.patterns.schemas._
import qq.droste.{Algebra, scheme}

trait AttributedFunction[In, Out] {

  def algebra: Algebra[SchemaF, In => Out]

  def modifier: Out => Attributed[Out]

  private lazy val compileFn = scheme.cata( Algebra.apply[SchemaF, In => Out] { schema => inp =>
    modifier(algebra(schema)(inp)).run(schema.attributes)
  })

  def makeFn(schema: Schema): In => Out = {
    compileFn(schema)
  }
}
