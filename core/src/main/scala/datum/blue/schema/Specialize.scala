package datum.blue.schema

import datum.blue.attributes.{AttributeKey, AttributeValue}
import turtles.Corecursive

import scala.collection.immutable.SortedMap

class Specialize[R](implicit R: Corecursive.Aux[R, SchemaF]) {
  def struct(
      fields: SortedMap[String, R],
      attributes: Map[AttributeKey, AttributeValue] = Map.empty
  ): R = {
    R.embed(StructF(fields, attributes))
  }

  def row(
      elements: Vector[R],
      attributes: Map[AttributeKey, AttributeValue] = Map.empty
  ): R = {
    R.embed(RowF(elements))
  }

  def value(
      tpe: Type,
      attributes: Map[AttributeKey, AttributeValue] = Map.empty
  )(implicit R: Corecursive.Aux[R, SchemaF]): R = {
    R.embed(ValueF(tpe, attributes))
  }
}

object Specialize {
  def apply[R](implicit R: Corecursive.Aux[R, SchemaF]) = new Specialize
}
