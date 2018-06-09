package datum.schema

import datum.FoldableFromTraverse
import cats.{Applicative, Traverse}
import cats.instances.sortedMap._
import cats.instances.string._

import scala.collection.immutable.SortedMap

sealed trait SchemaF[+R]
final case class StructF[R](fields: SortedMap[String, R]) extends SchemaF[R]
case object TextF extends SchemaF[Nothing]
case object IntF extends SchemaF[Nothing]

object SchemaF {
  val traverse: Traverse[SchemaF] = new FoldableFromTraverse[SchemaF] {
    override def traverse[G[_], A, B](fa: SchemaF[A])(f: A => G[B])(implicit G: Applicative[G]): G[SchemaF[B]] = fa match {
      case StructF(fields) =>
        val fs = Traverse[SortedMap[String, ?]].traverse[G, A, B](fields)(f)
        G.map(fs)(StructF.apply)

      case TextF => G.pure(TextF)

      case IntF => G.pure(IntF)
    }
  }
}