package datum.blue.schema

import datum.blue.attributes.{AttributeKey, AttributeValue}
import datum.blue.meta.MetaMap
import turtles.Corecursive

import scala.collection.immutable.SortedMap

class Specialize[R](implicit R: Corecursive.Aux[R, SchemaF]) {

  def struct(fields: (String, R) *)(attributes: MetaMap): R = {
    R.embed(StructF(SortedMap(fields:_*), attributes))
  }

  def row(
      elements: Vector[R],
      attributes: MetaMap = Map.empty
  ): R = {
    R.embed(RowF(elements))
  }

  def value(
      tpe: Type,
      attributes: MetaMap = Map.empty
  )(implicit R: Corecursive.Aux[R, SchemaF]): R = {
    R.embed(ValueF(tpe, attributes))
  }
}

object Specialize {
  def apply[R](implicit R: Corecursive.Aux[R, SchemaF]) = new Specialize
}
