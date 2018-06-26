package datum.red.path

import cats.{Applicative, Traverse}
import datum.FoldableFromTraverse

sealed trait PathF[+R]
case class NamedSegmentF[R](name: String, rest: R) extends PathF[R]
case class IndexSegmentF[R](idx: Int, header: Option[String], rest: R) extends PathF[R]
case class RootF[R](rest: R) extends PathF[R]
case object EndF extends PathF[Nothing]

object PathF {
  implicit val traverse: Traverse[PathF] = new FoldableFromTraverse[PathF]  {
    override def traverse[G[_], A, B](fa: PathF[A])(f: A => G[B])(implicit G: Applicative[G]): G[PathF[B]] = {
      fa match {
        case NamedSegmentF(n, a) => G.map(f(a)) { b => NamedSegmentF(n, b)}
        case IndexSegmentF(i, h, a) => G.map(f(a)) { b => IndexSegmentF(i, h, b)}
        case RootF(a) => G.map(f(a)) { b => RootF(b) }
        case EndF => G.pure(EndF)
      }
    }
  }
}
