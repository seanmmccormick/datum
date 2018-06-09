package datum

import cats.Traverse
import dataum.data._
import datum.helpers.Algebra
import datum.located._
import datum.path._

//import dataum.data._
//import schemes.Fix
//
//import scala.collection.immutable.SortedMap
//
//package object data {
//
//  type Data = Fix[DataF]
//
//  def struct(fields: SortedMap[String, Data]): Data = Fix.apply[DataF](StructValue(fields))
//
//  def text(value: String): Data = Fix.apply[DataF](TextValue(value))
//
//  def int(value: Int): Data = Fix.apply[DataF](IntValue(value))
//}

import cats.instances.function._

package object data {

  type Data = Path => Value

  def copy(src: Path, dest: Path)(inp: Data): Data = {
    case p if p == dest => inp(src)
  }

  val empty: Data = _ => NoValue

  type LocatedData = Located[Value]

  //implicit val stab: Traverse[LocatedData] = LocatedF.traverse[Value]

  val show: Algebra[LocatedF[Value, ?], String] = {
    case located.RootF(x) => "/" + x
    case located.StructF(fields) => fields.mkString("\n")
    case located.ArrayF(cols) => cols.mkString(",")
    case located.EntryF(x) => x.toString
  }
}