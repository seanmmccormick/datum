package datum.algebras.generic
import datum.patterns.attributes.Attributed
import datum.patterns.schemas._
import qq.droste.{Algebra, scheme}

trait AttributedFunction[In, Out] {

  def modify: Out => Attributed[Out]

  def base: Algebra[SchemaF, In => Out]

  val algebra: Algebra[SchemaF, In => Out] = Algebra {
    case v @ ValueF(_, attrs) =>
      in =>
        modify(base(v)(in)).run(attrs)

    case obj @ ObjF(_, attrs) =>
      in =>
        modify(base(obj)(in)).run(attrs)

    case row @ RowF(_, attrs) =>
      in =>
        modify(base(row)(in)).run(attrs)

    case arr @ ArrayF(_, attrs) =>
      in =>
        modify(base(arr)(in)).run(attrs)

    case union @ UnionF(_, attrs) =>
      in =>
        modify(base(union)(in)).run(attrs)
  }

  private val generator = scheme.cata(algebra)

  def makeFn(schema: Schema): In => Out = {
    generator(schema)
  }
}
