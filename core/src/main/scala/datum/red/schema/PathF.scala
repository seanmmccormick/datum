package datum.red.schema

import datum.FoldableFromTraverse

import cats.{Applicative, Traverse}

sealed trait PathF[+R]
case class StructSelectF[R](key: String, tail: R) extends PathF[R]
case object IntSelect extends PathF[Nothing]
case object TextSelect extends PathF[Nothing]

object PathF {
  implicit val traverse: Traverse[PathF] = new FoldableFromTraverse[PathF] {
    override def traverse[G[_], A, B](fa: PathF[A])(f: A => G[B])(implicit G: Applicative[G]): G[PathF[B]] = fa match {
      case StructSelectF(key, a) => G.map(f(a))(StructSelectF(key, _))
      case IntSelect => G.pure(IntSelect)
      case TextSelect => G.pure(TextSelect)
    }
  }
}