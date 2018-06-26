package datum.red.schema2

import cats.{Applicative, Traverse}
import cats.instances.sortedMap._
import cats.instances.order._

import datum.FoldableFromTraverse
import datum.red.path.Selector

import scala.collection.immutable.SortedMap

sealed trait SchemaReprF[+R]
final case class RootStruct[R](schema: R) extends SchemaReprF[R]
final case class StructF[R](fields: SortedMap[Selector, R]) extends SchemaReprF[R]
final case class ArrayF[R](schema: R) extends SchemaReprF[R]
case class DescriptionF(d: Description) extends SchemaReprF[Nothing]

object SchemaReprF {
  implicit val traverse: Traverse[SchemaReprF] = new FoldableFromTraverse[SchemaReprF] {
    override def traverse[G[_], A, B](fa: SchemaReprF[A])(f: A => G[B])(implicit G: Applicative[G]): G[SchemaReprF[B]] = fa match {
      case StructF(fields) =>
        val fs = Traverse[SortedMap[Selector, ?]].traverse[G, A, B](fields)(f)
        G.map(fs)(StructF.apply)

      case ArrayF(a) =>  G.map(f(a)) { b => ArrayF(b) }

      case RootStruct(a) => G.map(f(a)) { b => RootStruct(b) }

      case DescriptionF(d) => G.pure(DescriptionF(d))
    }
  }
}
