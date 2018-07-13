package datum.blue.transform

import cats.{Applicative, Traverse}
import datum.FoldableFromTraverse

import scala.collection.immutable.SortedMap
import cats.instances.sortedMap._
import cats.instances.string._

sealed trait TransformF[+R] extends Product with Serializable

final case class StructF[R](
  fields: SortedMap[String, R]
) extends TransformF[R]

//final case class TransformRowF[R](elements: Vector[R]) extends TransformF[R]

final case class ExplodeF[R](target: R) extends TransformF[R]

final case class RenameF[R](to: String, target: R) extends TransformF[R]

case object KeepF extends TransformF[Nothing]

case object DropF extends TransformF[Nothing]

object TransformF {
  implicit val traverse: Traverse[TransformF] = new FoldableFromTraverse[TransformF] {
    override def traverse[G[_], A, B](fa: TransformF[A])(f: A => G[B])(implicit G: Applicative[G]): G[TransformF[B]] = {
      fa match {
        case DropF => G.pure(DropF)
        case KeepF => G.pure(KeepF)
        case r @ RenameF(to, trg) =>
          G.map(f(trg)) { b =>
            RenameF(to, b)
          }
        case ExplodeF(trg) => G.map(f(trg))(ExplodeF.apply)
        case StructF(fields) =>
          val tm = Traverse[SortedMap[String, ?]].traverse(fields)(f)
          G.map(tm)(StructF.apply)
      }
    }
  }
}
