package datum.algebras.defaults.modifiers

import datum.patterns.attributes._
import datum.patterns.data
import datum.patterns.data.{Data, DataF, EmptyValue, RowValue}
import datum.patterns.schemas.{Column, RowF, SchemaF}

import qq.droste.Algebra
import qq.droste.data.{AttrF, Fix}

import scala.collection.mutable

/* Defaults Algebra Extension
 *
 * When used, will allow for the incoming data rows to have fewer columns then
 * what is specified in the Schema, inserting default values, when available,
 * and empty values otherwise.
 */

object EnableColumnDefaultExpansion {

  val key: AttributeKey = "enable-default-expansion".asAttributeKey

  def enable: (AttributeKey, Attribute) = key -> property(true)

  private def columnExpansionFn(
    inp: Data,
    dataValues: Vector[Data],
    columnFns: Vector[Column[Data => Data]]
  ): Data = {
    val results = mutable.Buffer.empty[Data]

    (dataValues zip columnFns).foreach {
      case (elem, schemaColumn) if elem == data.empty => results += schemaColumn.value(data.empty)
      case (elem, _)                                  => results += elem
    }

    (0 until columnFns.length - dataValues.length).foreach { idx =>
      val offset = idx + dataValues.length
      val fn = columnFns(offset).value
      results += fn(data.empty)
    }
    data.row(results.toVector)
  }

  val algebra: Algebra[AttrF[SchemaF, Data, ?], Data => Data] =
    Algebra[AttrF[SchemaF, Data, ?], Data => Data] {
      case AttrF(default, RowF(elements, attributes)) =>
        inp =>
          Fix.un[DataF](inp) match {
            case RowValue(values) => columnExpansionFn(inp, values, elements)
            case EmptyValue       => columnExpansionFn(inp, Vector.empty, elements)
            case _                => inp
          }
      case _ => identity
    }
}
