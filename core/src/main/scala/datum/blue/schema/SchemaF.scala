package datum.blue.schema

import datum.blue.attributes._

import cats.{Applicative, Traverse}
import cats.instances.list._
import cats.instances.vector._
import cats.instances.sortedMap._
import cats.instances.string._
import datum.FoldableFromTraverse

import scala.collection.immutable.SortedMap

sealed trait SchemaF[+R] extends Product with Serializable {
  def attributes: Map[AttributeKey, AttributeValue]
}

final case class StructF[R](
    fields: SortedMap[String, R],
    attributes: Map[AttributeKey, AttributeValue] = Map.empty
) extends SchemaF[R]

final case class ArrayF[R](
    element: R,
    attributes: Map[AttributeKey, AttributeValue] = Map.empty
) extends SchemaF[R]

final case class RowF[R](
    elements: Vector[R],
    attributes: Map[AttributeKey, AttributeValue] = Map.empty
) extends SchemaF[R]

final case class UnionF[R](
    alternatives: List[R],
    attributes: Map[AttributeKey, AttributeValue] = Map.empty
) extends SchemaF[R]

final case class ValueF(
    tpe: Type,
    attributes: Map[AttributeKey, AttributeValue] = Map.empty
) extends SchemaF[Nothing]

object SchemaF {
  implicit val traverse: Traverse[SchemaF] = new FoldableFromTraverse[SchemaF] {
    override def traverse[G[_], A, B](fa: SchemaF[A])(f: A => G[B])(
        implicit G: Applicative[G]): G[SchemaF[B]] = fa match {

      case v @ ValueF(_, _) => G.pure(v)

      case RowF(elems, attrs) =>
        val tl = Traverse[Vector].traverse(elems)(f)
        G.map(tl)(x => RowF(x, attrs))

      case UnionF(alts, attrs) =>
        val tl = Traverse[List].traverse(alts)(f)
        G.map(tl)(x => UnionF(x, attrs))

      case ArrayF(e, attrs) => G.map(f(e))(x => ArrayF(x, attrs))

      case StructF(fields, attrs) =>
        val tm = Traverse[SortedMap[String, ?]].traverse(fields)(f)
        G.map(tm)(x => StructF(x, attrs))
    }
  }
}