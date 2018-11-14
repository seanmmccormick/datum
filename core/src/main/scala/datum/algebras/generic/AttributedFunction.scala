package datum.algebras.generic
import cats.data.Reader
import datum.patterns.attributes.AttributeMap
import datum.patterns.schemas._
import qq.droste.{Algebra, scheme}

trait AttributedFunction[-In, Out] {

  def modify: Reader[AttributeMap, Out] => Reader[AttributeMap, Out]

  def base: Algebra[SchemaF, In => Out]

  private def run(attrs: AttributeMap)(initial: Out): Out = modify(Reader(_ => initial)).run(attrs)

  val algebra: Algebra[SchemaF, In => Out] = Algebra {
    case v @ ValueF(_, attrs) =>
      in =>
        run(attrs)(base(v)(in))

    case obj @ ObjF(_, attrs) =>
      in =>
        run(attrs)(base(obj)(in))

    case row @ RowF(_, attrs) =>
      in =>
        run(attrs)(base(row)(in))

    case arr @ ArrayF(_, attrs) =>
      in =>
        run(attrs)(base(arr)(in))

    case union @ UnionF(_, attrs) =>
      in =>
        run(attrs)(base(union)(in))
  }

  private val generator = scheme.cata(algebra)

  def makeFn(schema: Schema): In => Out = {
    generator(schema)
  }
}
