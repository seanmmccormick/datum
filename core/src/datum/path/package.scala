package datum

import cats.Order
import datum.helpers.{Algebra, Coalgebra}
import schemes._

package object path {

  sealed trait Selector
  case class Name(name: String) extends Selector
  case class Idx(idx: Int) extends Selector
  case object Nope extends Selector

  object Selector {
    implicit val order: Order[Selector] = new Order[Selector] {
      override def compare(x: Selector, y: Selector): Int = (x, y) match {
        case (Idx(l), Idx(r)) => l.compare(r)
        case (Name(l), Name(r)) => l.compare(r)
        case (Idx(_), _) => 1
        case (_, Idx(_)) => -1
        case (Name(_), _) => 1
        case (_, Name(_)) => -1
        case _ => 0
      }
    }
  }

  type Path = Fix[PathF]

  def named[A](name: String, rest: Path): Path =
    Fix.apply[PathF](NamedSegmentF(name, rest))

  def idx[A](idx: Int, header: Option[String], rest: Path): Path =
    Fix.apply[PathF](IndexSegmentF(idx, header, rest))

  def root(rest: Path): Path = Fix.apply[PathF](RootF(rest))

  val end: Path = Fix.apply[PathF](EndF)

  def head(p: Path): Selector = p match {
    case NamedSegmentF(n, _) => Name(n)
    case IndexSegmentF(idx, _, _) => Idx(idx)
    case r: RootF[Path] => head(r.rest)
    //case EndF => Nope
  }

  def tail(p: Path): Path = p match {
    case n: NamedSegmentF[Path] => n.rest
    case i: IndexSegmentF[Path] => i.rest
    case r: RootF[Path] => tail(r.rest)
    //case EndF => end

  }
}
