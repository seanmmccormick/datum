package datum.patterns.schemas

import datum.patterns.attributes.{Attribute, AttributeKey}

import cats.{Applicative, Traverse}
import cats.instances.sortedMap._
import cats.instances.string._
import cats.instances.vector._
import cats.instances.list._
import qq.droste.util.DefaultTraverse
import scala.collection.immutable.SortedMap

sealed trait SchemaF[+R] extends Product with Serializable {
  def attributes: Map[AttributeKey, Attribute]
}

final case class ObjF[R](
  fields: SortedMap[String, R],
  attributes: Map[AttributeKey, Attribute] = Map.empty
) extends SchemaF[R]

final case class RowF[R](
  elements: Vector[Column[R]],
  attributes: Map[AttributeKey, Attribute] = Map.empty
) extends SchemaF[R]

final case class ArrayF[R](
  element: R,
  attributes: Map[AttributeKey, Attribute] = Map.empty
) extends SchemaF[R]

final case class UnionF[R](
  alternatives: List[R],
  attributes: Map[AttributeKey, Attribute] = Map.empty
) extends SchemaF[R]

final case class ValueF(
  tpe: Type,
  attributes: Map[AttributeKey, Attribute] = Map.empty
) extends SchemaF[Nothing]

object SchemaF {
  implicit val traverse: Traverse[SchemaF] = new DefaultTraverse[SchemaF] {
    override def traverse[G[_], A, B](fa: SchemaF[A])(f: A => G[B])(implicit G: Applicative[G]): G[SchemaF[B]] = {
      fa match {
        case v @ ValueF(_, _) => G.pure(v)

        case RowF(elems, attrs) =>
          val tl = Traverse[Vector].traverse(elems) { e =>
            G.map(f(e.value))(Column.apply(_, e.header))
          }
          G.map(tl)(x => RowF(x, attrs))

        case ObjF(fields, meta) =>
          val tm = Traverse[SortedMap[String, ?]].traverse(fields)(f)
          G.map(tm)(x => ObjF(x, meta))

        case UnionF(alts, meta) =>
          val tl = Traverse[List].traverse(alts)(f)
          G.map(tl)(x => UnionF(x, meta))

        case ArrayF(e, meta) => G.map(f(e))(x => ArrayF(x, meta))
      }
    }
  }
}
