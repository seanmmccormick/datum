package datum.blue.data

import turtles.Corecursive

import scala.collection.immutable.SortedMap

class Specialize[R](implicit R: Corecursive.Aux[R, DataF]) {

  def struct(fields: (String, R)*): R = {
    R.embed(StructDataF(SortedMap(fields: _*)))
  }

  def row(
      elements: Vector[R]
  ): R = {
    R.embed(RowDataF(elements))
  }

  def text(value: String): R = {
    R.embed(TextDataF(value))
  }

  def real(value: Double): R = {
    R.embed(RealDataF(value))
  }

  def integer(value: Long): R = {
    R.embed(IntegerDataF(value))
  }
}

object Specialize {
  def apply[R](implicit R: Corecursive.Aux[R, DataF]) = new Specialize
}
