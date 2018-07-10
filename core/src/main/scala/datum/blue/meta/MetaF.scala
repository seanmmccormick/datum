package datum.blue.meta

import cats.Functor

sealed trait MetaF[+R]
case class AndF[R](lhs: R, rhs: R) extends MetaF[R]
case class OrF[R](lhs: R, rhs: R) extends MetaF[R]
case class LabelF[R](name: String, value: R) extends MetaF[R]
case class TextPropertyF(value: String) extends MetaF[Nothing]
case class NumericPropertyF(value: Double) extends MetaF[Nothing]
case class BooleanPropertyF(value: Boolean) extends MetaF[Nothing]

object MetaF {
  implicit val functor: Functor[MetaF] = new Functor[MetaF] {
    override def map[A, B](fa: MetaF[A])(f: A => B): MetaF[B] = fa match {
      case v @ BooleanPropertyF(_) => v
      case v @ NumericPropertyF(_) => v
      case v @ TextPropertyF(_) => v
      case LabelF(n, a) => LabelF(n, f(a))
      case OrF(a1, a2) => OrF(f(a1), f(a2))
      case AndF(a1, a2) => AndF(f(a1), f(a2))
    }
  }
}
