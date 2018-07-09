package datum.blue.ops

import alleycats.Empty
import datum.blue.data._
import datum.blue.{attributes, data, schema}
import datum.blue.schema._
import turtles._
import datum.blue.attributes.{AttributeKey, AttributeValue, Attributes}

object Corresponds {

  type =>?[-A, +B] = PartialFunction[A, B]

  def algebra[Data: Empty](
    check: (DataF[Data], Attributes) =>? Boolean
  )(implicit Data: Recursive.Aux[Data, DataF]): Algebra[SchemaF, Data => Boolean] = {

    def checkOrDefault(d: DataF[Data], attrs: Attributes, default: Boolean): Boolean = {
      check.applyOrElse[(DataF[Data], Attributes), Boolean]((d, attrs), _ => default)
    }

    {
      case schema.ValueF(TextType, attrs) =>
        inp =>
          Data.project(inp) match {
            case d @ data.TextDataF(_) => checkOrDefault(d, attrs, default = true)
            case d                     => checkOrDefault(d, attrs, default = false)
          }

      case schema.ValueF(IntegerType, attrs) =>
        inp =>
          Data.project(inp) match {
            case d @ data.IntegerDataF(_) => checkOrDefault(d, attrs, default = true)
            case d                        => checkOrDefault(d, attrs, default = false)
          }

      case schema.ValueF(RealType, attrs) =>
        inp =>
          Data.project(inp) match {
            case d @ data.RealDataF(_) => checkOrDefault(d, attrs, default = true)
            case d                     => checkOrDefault(d, attrs, default = false)
          }

      case schema.RowF(elements, attrs) =>
        inp =>
          Data.project(inp) match {
            case d @ RowDataF(values) if values.length == elements.length =>
              val default = values.zip(elements).forall { case (e, corresponds) => corresponds(e) }
              checkOrDefault(d, attrs, default)
            case d => checkOrDefault(d, attrs, default = false)
          }

      case schema.StructF(fields, attrs) =>
        inp =>
          Data.project(inp) match {
            case d @ StructDataF(dataFields) =>
              val default = fields.forall {
                case (key, corresponds) =>
                  corresponds(dataFields.getOrElse(key, Empty[Data].empty))
              }
              checkOrDefault(d, attrs, default)

            case d => checkOrDefault(d, attrs, default = false)
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
