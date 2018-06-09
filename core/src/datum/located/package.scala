package datum

import cats.instances.order._

import datum.path.Selector
import schemes._

import scala.collection.immutable.SortedMap

package object located {

  type Located[V] = Fix[LocatedF[V, ?]]

  def root(of: Located[Nothing]): Located[Nothing] = Fix.apply[LocatedF[Nothing, ?]](RootF(of))

  def struct(fields: (Selector, Located[Nothing])* ): Located[Nothing] =
    Fix.apply[LocatedF[Nothing, ?]](StructF(SortedMap(fields:_*)))

  def array[V](of: List[Located[V]]): Located[V] = {
    Fix.apply[LocatedF[V, ?]](ArrayF(of))
  }

  def entry[V](value: V): Located[V] = fix(EntryF[V](value))

  def fix[V](inp: LocatedF[V, Located[V]]): Located[V] = Fix.apply[LocatedF[V, ?]](inp)
}
