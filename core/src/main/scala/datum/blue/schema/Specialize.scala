package datum.blue.schema

import datum.blue.attributes.Attributes
import turtles.Corecursive

import scala.collection.immutable.SortedMap

class Specialize[R](implicit R: Corecursive.Aux[R, SchemaF]) {

  def struct(fields: (String, R)*)(attributes: Attributes): R = {
    R.embed(StructF(SortedMap(fields: _*), attributes))
  }

  def row(elements: R*)(attributes: Attributes = Map.empty): R = {
    R.embed(RowF(Vector(elements:_*)))
  }

  def value(
    tpe: Type,
    attributes: Attributes = Map.empty
  )(implicit R: Corecursive.Aux[R, SchemaF]): R = {
    R.embed(ValueF(tpe, attributes))
  }
}

object Specialize {
  def apply[R](implicit R: Corecursive.Aux[R, SchemaF]) = new Specialize
}
