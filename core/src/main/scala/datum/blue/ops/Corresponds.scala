package datum.blue.ops

import cats.{Applicative, Monoid}
import datum.blue.data._
import datum.blue.{attributes, data, schema}
import datum.blue.schema.{IntegerType, SchemaF, TextType}
import turtles._
import cats.syntax.applicative._

object Corresponds {

  type Alg[A] = Algebra[DataF, A]
  def omg[A](implicit F: Applicative[DataF], A: Monoid[A]): Algebra[SchemaF, Alg[A]] = {
    case schema.StructF(schemaFields, _) => {
      case data.StructDataF(dataFields) =>
        val waaat = schemaFields.map { case (key, ermg) =>
          val hrm = dataFields.apply(key)
          ermg(F.pure(hrm))
        }
        Monoid[A].combineAll(waaat)
        ???
    }
  }

  def algebra[Data](implicit Data: Recursive.Aux[Data, DataF])
    : Algebra[SchemaF, Data => Boolean] = {
    case schema.ValueF(TextType, attrs) =>
      inp =>
        Data.project(inp) match {
          case data.TextDataF(_) => true
          case _                 => attributes.isOptional(attrs)
        }

    case schema.ValueF(IntegerType, attrs) =>
      inp =>
        Data.project(inp) match {
          case data.IntegerDataF(_) => true
          case _                    => attributes.isOptional(attrs)
        }

    case schema.RowF(elements, attrs) =>
      inp =>
        Data.project(inp) match {
          case RowDataF(values) if values.length == elements.length =>
            values.zip(elements).forall { case (e, f) => f(e) }
          case _ => attributes.isOptional(attrs)
        }

    case schema.StructF(fields, attrs) =>
      inp =>
        Data.project(inp) match {
          case StructDataF(dataFields) =>
            fields.forall {
              case (key, corresponds) =>
                val z = dataFields.getOrElse[Data](key, inp) // this seems silly -- im using inp as substitute for a "null" value
                corresponds(z)
            }
          case _ => attributes.isOptional(attrs)
        }

    case omg =>
      println("========== FAILED TO MATCH ==========")
      pprint.pprintln(omg)
      x =>
        false
  }

  def apply[Data, Schema](sch: Schema)(data: Data)(
      implicit Data: Recursive.Aux[Data, DataF],
      Schema: Recursive.Aux[Schema, SchemaF]
  ): Boolean = {
    println(" ======== START ======")
    val f = Schema.cata(sch)(algebra[Data](Data))
    f(data)
  }
}
