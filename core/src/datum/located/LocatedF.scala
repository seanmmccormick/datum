package datum.located

import cats.{Applicative, Traverse}
import cats.instances.sortedMap._
import cats.instances.list._
import cats.instances.order._
import datum.FoldableFromTraverse
import datum.path.Selector

import scala.collection.immutable.SortedMap

sealed trait LocatedF[V, +R]
final case class RootF[V, R](schema: R) extends LocatedF[V, R]
final case class StructF[V, R](fields: SortedMap[Selector, R]) extends LocatedF[V, R]
final case class ArrayF[V, R](columns: List[R]) extends LocatedF[V, R]
final case class EntryF[V](entry: V) extends LocatedF[V, Nothing]

object LocatedF {
  implicit def traverse[V]: Traverse[LocatedF[V, ?]] = new FoldableFromTraverse[LocatedF[V, ?]] {
    override def traverse[G[_], A, B](fa: LocatedF[V, A])(f: A => G[B])(implicit G: Applicative[G]): G[LocatedF[V, B]] = fa match {
      case StructF(fields) =>
        val fs = Traverse[SortedMap[Selector, ?]].traverse[G, A, B](fields)(f)
        G.map(fs)(StructF.apply)

      case ArrayF(cols) =>
        val fs = Traverse[List].traverse[G, A, B](cols)(f)
        G.map(fs)(ArrayF.apply)

      case RootF(a) => G.map(f(a)) { b => RootF(b) }

      case EntryF(d) => G.pure(EntryF(d))
    }
  }
}