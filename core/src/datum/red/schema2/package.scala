package datum.red

import cats.{Applicative, Traverse}
import cats.instances.order._

import datum.red.helpers.Algebra
import datum.red.path._
import schemes.{Fix, Schemes}

import scala.collection.immutable.SortedMap

package object schema2 {

  type Schema = Path => Description

  type SchemaRepr = Fix[SchemaReprF]

  def root(of: SchemaRepr): SchemaRepr = Fix.apply[SchemaReprF](RootStruct(of))

  def struct(fields: (Selector, SchemaRepr)* ): SchemaRepr =
    Fix.apply[SchemaReprF](StructF(SortedMap(fields:_*)))

  def array(of: SchemaRepr): SchemaRepr = {
    Fix.apply[SchemaReprF](ArrayF(of))
  }

  val int: SchemaRepr = Fix.apply[SchemaReprF](DescriptionF(IntD))

  val text: SchemaRepr = Fix.apply[SchemaReprF](DescriptionF(TextD))

  val toSchema: Algebra[SchemaReprF, Schema] = {
    case StructF(fields) => p =>
      val h = head(p)
      val next: Schema = fields.getOrElse(h, _ => NullD)
      next(tail(p))

    case DescriptionF(d) => {
      case e if e == end => d
      case _ => NullD
    }
  }

  def schema(inp: SchemaRepr): Schema = Schemes.cata[SchemaReprF, Schema](inp)(toSchema)
}