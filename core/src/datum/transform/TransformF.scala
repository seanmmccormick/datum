package datum.transform

import datum.schema

import scala.collection.immutable.SortedMap

sealed trait Transform
case class StructFrom(fields: SortedMap[String, Transform]) extends Transform
case class Copy(src: schema.Path) extends Transform

object Transform {
  def struct(omg: (String, Transform)* ): Transform = {
    StructFrom(SortedMap(omg:_*))
  }
}







////
case class Selection(path: schema.Path, as: Option[String])

sealed trait SQL
case class sql(selections: List[Selection]) extends SQL

object SQL {
  implicit def pathAsSelection(p: schema.Path): Selection = Selection(p, None)
}

