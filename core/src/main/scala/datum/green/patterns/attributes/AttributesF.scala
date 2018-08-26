package datum.green.patterns.attributes

import cats.{Applicative, Traverse}
import qq.droste.util.DefaultTraverse

sealed trait AttributesF[+R] extends Product with Serializable

case class AndF[R](lhs: R, rhs: R) extends AttributesF[R]
case class OrF[R](lhs: R, rhs: R) extends AttributesF[R]
case class LabelF[R](name: String, value: R) extends AttributesF[R]
case class TextPropertyF(value: String) extends AttributesF[Nothing]
case class NumericPropertyF(value: Double) extends AttributesF[Nothing]
case class BooleanPropertyF(value: Boolean) extends AttributesF[Nothing]

object AttributesF {
  implicit val traverse: Traverse[AttributesF] = new DefaultTraverse[AttributesF] {
    override def traverse[G[_], A, B](fa: AttributesF[A])(f: A => G[B])(
      implicit G: Applicative[G]): G[AttributesF[B]] = {
      fa match {
        case v @ BooleanPropertyF(_) => G.pure(v)
        case v @ NumericPropertyF(_) => G.pure(v)
        case v @ TextPropertyF(_)    => G.pure(v)
        case LabelF(n, a) =>
          G.map(f(a)) { b =>
            LabelF(n, b)
          }
        case OrF(a1, a2)  => G.map2(f(a1), f(a2)) { case (b1, b2) => OrF(b1, b2) }
        case AndF(a1, a2) => G.map2(f(a1), f(a2)) { case (b1, b2) => AndF(b1, b2) }
      }
    }
  }
}
