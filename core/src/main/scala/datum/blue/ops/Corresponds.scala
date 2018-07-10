package datum.blue.ops

import alleycats.Empty
import datum.blue.data._
import datum.blue.{data, meta, schema}
import datum.blue.schema._
import turtles._
import datum.blue.meta.MetaMap

object Corresponds {

  type =>?[-A, +B] = PartialFunction[A, B]

  def algebra[Data: Empty](
    check: (DataF[Data], MetaMap) =>? Boolean
  )(implicit Data: Recursive.Aux[Data, DataF]): Algebra[SchemaF, Data => Boolean] = {

    def checkOrDefault(d: DataF[Data], attrs: MetaMap, default: Boolean): Boolean = {
      check.applyOrElse[(DataF[Data], MetaMap), Boolean]((d, attrs), _ => default)
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
              val default = values.zip(elements).forall { case (e, corresponds) => corresponds(e) }
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
    checking: PartialFunction[(DataF[Data], MetaMap), Boolean])(
    implicit Data: Recursive.Aux[Data, DataF],
    Schema: Recursive.Aux[Schema, SchemaF]
  ): Boolean = {
    val f = Schema.cata(sch)(algebra[Data](checking))
    f(data)
  }

  object checks {
    def optional[Data]: (DataF[Data], MetaMap) =>? Boolean = {
      //case (_, attr) if attributes.isOptional(attr) => true
      case (_, attr) if meta.isOptional(attr) => true
    }

    def default[Data]: (DataF[Data], MetaMap) =>? Boolean = optional
  }
}
