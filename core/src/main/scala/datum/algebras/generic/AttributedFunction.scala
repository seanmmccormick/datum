package datum.algebras.generic
import datum.patterns.attributes.Attributed
import datum.patterns.schemas._
import qq.droste.data.{Attr, AttrF, Fix}
import qq.droste.{Algebra, scheme}
import qq.droste.data.prelude._

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

object Wurt {
  type WithSchema[In, Out] = cats.data.Reader[SchemaF[In => Out], Out]

}
import Wurt._

trait AnnotatedFunction[Annotation, In, Out] {

  def modify: Out => WithSchema[In, Out]

  def base: Algebra[AttrF[SchemaF, Annotation, ?], In => Out]

  val algebra: Algebra[AttrF[SchemaF, Annotation, ?], In => Out] =
    Algebra[AttrF[SchemaF, Annotation, ?], In => Out] { v => in =>
      modify(base(v)(in)).run(v.lower)
    }

  private val generator = scheme.cata(algebra)

  def makeFn(annotated: Attr[SchemaF, Annotation]): In => Out = {
    generator(annotated)
  }
}
