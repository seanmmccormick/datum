package dataum.data

//import cats.{Applicative, Traverse}
//import cats.instances.sortedMap._
//import cats.instances.string._
//import datum.FoldableFromTraverse
//
//import scala.collection.immutable.SortedMap
//
//sealed trait DataF[+R]
//final case class StructValue[R](fields: SortedMap[String, R]) extends DataF[R]
//final case class TextValue(value: String) extends DataF[Nothing]
//final case class IntValue(value: Int) extends DataF[Nothing]
//
//object DataF {
//  implicit val traverse: Traverse[DataF] = new FoldableFromTraverse[DataF] {
//    override def traverse[G[_], A, B](fa: DataF[A])(f: A => G[B])(implicit G: Applicative[G]): G[DataF[B]] = fa match {
//      case StructValue(fields) =>
//        val traversed = Traverse[SortedMap[String, ?]].traverse(fields)(f)
//        G.map(traversed)(StructValue.apply)
//
//      case v @ TextValue(_) => G.pure(v)
//
//      case v @ IntValue(_) => G.pure(v)
//    }
//  }
//}

sealed trait Value
case class IntValue(v: Int) extends Value
case class TextValue(v: String) extends Value
case object NoValue extends Value