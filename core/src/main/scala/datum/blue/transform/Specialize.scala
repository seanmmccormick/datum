package datum.blue.transform

import turtles.Corecursive

import scala.collection.immutable.SortedMap

class Specialize[R](implicit R: Corecursive.Aux[R, TransformF]) {

  def struct(fields: (String, R)*): R = {
    R.embed(StructF(SortedMap(fields:_*)))
  }

  def explode(target: R): R = {
    R.embed(ExplodeF(target))
  }

  def rename(to: String, target: R): R = {
    R.embed(RenameF(to, target))
  }

  def keep: R = {
    R.embed(KeepF)
  }

  def drop: R = {
    R.embed(DropF)
  }
}

object Specialize {
  def apply[R](implicit R: Corecursive.Aux[R, TransformF]) = new Specialize
}