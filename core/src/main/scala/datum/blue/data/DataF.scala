package datum.blue.data

import cats.{Applicative, Traverse}
import cats.instances.list._
import cats.instances.option._
import cats.instances.vector._
import cats.instances.sortedMap._
import cats.instances.string._
import datum.FoldableFromTraverse

import scala.collection.immutable.SortedMap

sealed trait DataF[+R] extends Product with Serializable

final case class StructDataF[R](fields: SortedMap[String, R]) extends DataF[R]
final case class RowDataF[R](values: Vector[R]) extends DataF[R]

// union ???

final case class OptionalDataF[R](value: Option[R]) extends DataF[R]
final case class IntegerDataF(value: Long) extends DataF[Nothing]
final case class RealDataF(value: Double) extends DataF[Nothing]
final case class TextDataF(value: String) extends DataF[Nothing]

object DataF {
  implicit val traverse: Traverse[DataF] = new FoldableFromTraverse[DataF] {
    override def traverse[G[_], A, B](fa: DataF[A])(f: A => G[B])(
        implicit G: Applicative[G]
    ): G[DataF[B]] = fa match {

      case StructDataF(fields) =>
        val tm = Traverse[SortedMap[String, ?]].traverse(fields)(f)
        G.map(tm)(StructDataF.apply)

      case RowDataF(vs) =>
        val tv = Traverse[Vector].traverse(vs)(f)
        G.map(tv)(RowDataF.apply)

      case OptionalDataF(opt) =>
        val to = Traverse[Option].traverse(opt)(f)
        G.map(to)(OptionalDataF.apply)

      case v @ IntegerDataF(_) => G.pure(v)
      case v @ RealDataF(_)    => G.pure(v)
      case v @ TextDataF(_)    => G.pure(v)

    }
  }
}
