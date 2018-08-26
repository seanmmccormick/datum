package datum.green.patterns.data

import cats.{Applicative, Traverse}
import qq.droste.util.DefaultTraverse
import cats.instances.sortedMap._
import cats.instances.vector._
import cats.instances.string._

import scala.collection.immutable.SortedMap

sealed trait DataF[+R] extends Product with Serializable

final case class StructDataF[R](fields: SortedMap[String, R]) extends DataF[R]
final case class RowDataF[R](values: Vector[R]) extends DataF[R]
final case class IntegerDataF(value: Long) extends DataF[Nothing]
final case class RealDataF(value: Double) extends DataF[Nothing]
final case class TextDataF(value: String) extends DataF[Nothing]
final case class BooleanDataF(value: Boolean) extends DataF[Nothing]
case object EmptyDataF extends DataF[Nothing]

object DataF {
  implicit val traverse: Traverse[DataF] = new DefaultTraverse[DataF] {
    override def traverse[G[_], A, B](fa: DataF[A])(f: A => G[B])(implicit G: Applicative[G]): G[DataF[B]] = {
      fa match {
        case StructDataF(fields) =>
          val tm = Traverse[SortedMap[String, ?]].traverse(fields)(f)
          G.map(tm)(StructDataF.apply)

        case RowDataF(vs) =>
          val tv = Traverse[Vector].traverse(vs)(f)
          G.map(tv)(RowDataF.apply)

        case v @ IntegerDataF(_) => G.pure(v)
        case v @ RealDataF(_)    => G.pure(v)
        case v @ TextDataF(_)    => G.pure(v)
        case v @ BooleanDataF(_) => G.pure(v)
        case n @ EmptyDataF      => G.pure(n)

      }
    }
  }
}
