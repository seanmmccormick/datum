package datum.patterns.attributes

import cats.{Applicative, Traverse}
import cats.instances.vector._

import qq.droste.util.DefaultTraverse

sealed trait AttributesF[+R] extends Product with Serializable

case class And[R](lhs: R, rhs: R) extends AttributesF[R]
case class Or[R](lhs: R, rhs: R) extends AttributesF[R]
case class Label[R](name: String, value: R) extends AttributesF[R]
case class Collection[R](values: Vector[R]) extends AttributesF[R]
case class Property(value: String) extends AttributesF[Nothing]
case class NumProperty(value: Double) extends AttributesF[Nothing]
case class BoolProperty(value: Boolean) extends AttributesF[Nothing]

object AttributesF {
  implicit val traverse: Traverse[AttributesF] = new DefaultTraverse[AttributesF] {
    override def traverse[G[_], A, B](fa: AttributesF[A])(f: A => G[B])(
      implicit G: Applicative[G]): G[AttributesF[B]] = {
      fa match {
        case v @ BoolProperty(_) => G.pure(v)
        case v @ NumProperty(_) => G.pure(v)
        case v @ Property(_)    => G.pure(v)
        case Label(n, a) =>
          G.map(f(a)) { b =>
            Label(n, b)
          }
        case Or(a1, a2)  => G.map2(f(a1), f(a2)) { case (b1, b2) => Or(b1, b2) }
        case And(a1, a2) => G.map2(f(a1), f(a2)) { case (b1, b2) => And(b1, b2) }
        case Collection(vs) =>
          val tv = Traverse[Vector].traverse(vs)(f)
          G.map(tv)(Collection.apply)
      }
    }
  }
}
