package datum.blue.ops

import alleycats.Empty
import datum.=>?
import datum.blue.data._
import datum.blue.{attributes, data, schema}
import datum.blue.schema._
import turtles._
import datum.blue.attributes.Attributes

object Corresponds {

  def algebra[Data: Empty](
    checks: (DataF[Data], Attributes) =>? Boolean
  )(implicit Data: Recursive.Aux[Data, DataF]): Algebra[SchemaF, Data => Boolean] = {

    def checkOrDefault(d: DataF[Data], attrs: Attributes, default: Boolean): Boolean = {
      checks.applyOrElse[(DataF[Data], Attributes), Boolean]((d, attrs), _ => default)
    }

    {
      case schema.ValueF(TextType, meta) =>
        inp =>
          Data.project(inp) match {
            case d @ data.TextDataF(_) => checkOrDefault(d, meta, default = true)
            case d                     => checkOrDefault(d, meta, default = false)
          }

      case schema.ValueF(IntegerType, meta) =>
        inp =>
          Data.project(inp) match {
            case d @ data.IntegerDataF(_) => checkOrDefault(d, meta, default = true)
            case d                        => checkOrDefault(d, meta, default = false)
          }

      case schema.ValueF(RealType, meta) =>
        inp =>
          Data.project(inp) match {
            case d @ data.RealDataF(_) => checkOrDefault(d, meta, default = true)
            case d                     => checkOrDefault(d, meta, default = false)
          }

      case schema.RowF(elements, meta) =>
        inp =>
          Data.project(inp) match {
            case d @ RowDataF(values) if values.length == elements.length =>
              val default = values.zip(elements).forall { case (e, col) => col.value(e) }
              checkOrDefault(d, meta, default)
            case d => checkOrDefault(d, meta, default = false)
          }

      case schema.StructF(fields, meta) =>
        inp =>
          Data.project(inp) match {
            case d @ StructDataF(dataFields) =>
              val default = fields.forall {
                case (key, corresponds) =>
                  corresponds(dataFields.getOrElse(key, Empty[Data].empty))
              }
              checkOrDefault(d, meta, default)

            case d => checkOrDefault(d, meta, default = false)
          }
    }
  }

  def apply[Data: Empty, Schema](sch: Schema)(data: Data)(
    implicit Data: Recursive.Aux[Data, DataF],
    Schema: Recursive.Aux[Schema, SchemaF]
  ): Boolean = {
    val f = Schema.cata(sch)(algebra[Data](checks.default))
    f(data)
  }

  def partial[Data: Empty, Schema](sch: Schema, data: Data)(
    checking: PartialFunction[(DataF[Data], Attributes), Boolean])(
    implicit Data: Recursive.Aux[Data, DataF],
    Schema: Recursive.Aux[Schema, SchemaF]
  ): Boolean = {
    val f = Schema.cata(sch)(algebra[Data](checking))
    f(data)
  }

  object checks {

    def optional[Data]: (DataF[Data], Attributes) =>? Boolean = {
      case (_, attr) if attributes.isOptional(attr) => true
    }

    def default[Data]: (DataF[Data], Attributes) =>? Boolean = optional
  }
}
