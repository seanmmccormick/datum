package datum.blue.attributes

import cats.{Applicative, Functor, Traverse}
import datum.FoldableFromTraverse

sealed trait AttrF[+R] extends Product with Serializable

case class AndF[R](lhs: R, rhs: R) extends AttrF[R]
case class OrF[R](lhs: R, rhs: R) extends AttrF[R]
case class LabelF[R](name: String, value: R) extends AttrF[R]
case class TextPropertyF(value: String) extends AttrF[Nothing]
case class NumericPropertyF(value: Double) extends AttrF[Nothing]
case class BooleanPropertyF(value: Boolean) extends AttrF[Nothing]

object AttrF {
  implicit val traverse: Traverse[AttrF] = new FoldableFromTraverse[AttrF] {
    override def traverse[G[_], A, B](fa: AttrF[A])(f: A => G[B])(implicit G: Applicative[G]): G[AttrF[B]] =
      fa match {
        case v @ BooleanPropertyF(_) => G.pure(v)
        case v @ NumericPropertyF(_) => G.pure(v)
        case v @ TextPropertyF(_)    => G.pure(v)
        case LabelF(n, a)            => G.map(f(a)) { case b => LabelF(n, b) }
        case OrF(a1, a2)             => G.map2(f(a1), f(a2)) { case (b1, b2) => OrF(b1, b2) }
        case AndF(a1, a2)            => G.map2(f(a1), f(a2)) { case (b1, b2) => AndF(b1, b2) }
      }
  }
}
