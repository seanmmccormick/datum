package datum.blue.ops

import datum.blue.schema
import datum.blue.transform
import datum.blue.transform.TransformF
import turtles.{Algebra, Birecursive, Recursive}
import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.option._
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

    case transform.StructF(fields) =>
      inp =>
        Schema.project(inp) match {
          case schema.StructF(inpFields, attrs) =>
//            fields.toList
//              .traverse[Option, (String, Schema)] {
//                case (k, v) =>
//                  inpFields.get(k).flatMap(v).map((k, _))
//              }
//              .map { collected =>
//                Schema.embed(schema.StructF(SortedMap(collected: _*), attrs))
//              }
            val collected = inpFields.toList.flatMap {
              case (k, v) =>
                fields
                  .get(k)
                  .flatMap(transformFunc => transformFunc(v))
                  .orElse { if (keepByDefault) Some(v) else None }
                  .map((k, _))
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

  def apply[Transform, Schema](transform: Transform)(sch: Schema)(
    implicit Transform: Recursive.Aux[Transform, TransformF],
    Schema: Birecursive.Aux[Schema, SchemaF]
  ): Option[Schema] = {
    val f = Transform.cata(transform)(algebra())
    f(sch)
  }
}
