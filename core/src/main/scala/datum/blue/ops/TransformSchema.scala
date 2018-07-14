package datum.blue.ops

import datum.blue.schema
import datum.blue.transform
import datum.blue.transform.TransformF
import turtles.{Algebra, Birecursive, Recursive}
import datum.blue.schema.SchemaF

import scala.collection.immutable.SortedMap

object TransformSchema {

  def algebra[Schema](keepByDefault: Boolean = false)(
    implicit Schema: Birecursive.Aux[Schema, schema.SchemaF]
  ): Algebra[TransformF, Schema => Option[Schema]] = {
    case transform.KeepF => Option.apply

    case transform.DropF =>
      _ =>
        Option.empty

    case transform.StructF(transforms) =>
      inp =>
        Schema.project(inp) match {
          case schema.StructF(inpFields, attrs) =>
            val collected = inpFields.toList.flatMap {
              case (k, v) =>
                val r = transforms.get(k) match {
                  case Some(tf) => tf(v)
                  case None if keepByDefault => Some(v)
                  case _ => None
                }
                r.map((k, _))
            }
            Some(Schema.embed(schema.StructF(SortedMap(collected: _*), attrs)))

          case _ => None
        }

    case transform.ExplodeF(f) =>
      inp =>
        f(inp).flatMap { sch =>
          Schema.project(sch) match {
            case schema.StructF(fields, attrs) => Option(Schema.embed(schema.RowF(fields.values.toVector, attrs)))
            case _                             => None
          }
        }
  }

  def apply[Transform, Schema](transform: Transform, keepByDefault: Boolean = false)(sch: Schema)(
    implicit Transform: Recursive.Aux[Transform, TransformF],
    Schema: Birecursive.Aux[Schema, SchemaF]
  ): Option[Schema] = {
    val f = Transform.cata(transform)(algebra(keepByDefault))
    f(sch)
  }
}
