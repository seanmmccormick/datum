package datum.blue.attributes

import cats.Functor

sealed trait AttrF[+R]
case class AndF[R](lhs: R, rhs: R) extends AttrF[R]
case class OrF[R](lhs: R, rhs: R) extends AttrF[R]
case class LabelF[R](name: String, value: R) extends AttrF[R]
case class TextPropertyF(value: String) extends AttrF[Nothing]
case class NumericPropertyF(value: Double) extends AttrF[Nothing]
case class BooleanPropertyF(value: Boolean) extends AttrF[Nothing]

object AttrF {
  implicit val functor: Functor[AttrF] = new Functor[AttrF] {
    override def map[A, B](fa: AttrF[A])(f: A => B): AttrF[B] = fa match {
      case v @ BooleanPropertyF(_) => v
      case v @ NumericPropertyF(_) => v
      case v @ TextPropertyF(_) => v
      case LabelF(n, a) => LabelF(n, f(a))
      case OrF(a1, a2) => OrF(f(a1), f(a2))
      case AndF(a1, a2) => AndF(f(a1), f(a2))
    }
  }
}
