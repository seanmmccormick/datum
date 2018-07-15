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

  def select(field: String, target: R): R = {
    R.embed(SelectFieldF(field, target))
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
  //def apply2[F[_[_]]](implicit F: Corecursive.Aux[F[TransformF], TransformF]) = new Specialize[F[TransformF]]
}