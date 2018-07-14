package datum.blue.ops

import datum.blue.data
import datum.blue.data.DataF
import datum.blue.ops.TransformSchema.algebra
import datum.blue.schema.SchemaF
import datum.blue.transform
import datum.blue.transform.TransformF
import turtles.{Algebra, Birecursive, Recursive}

import scala.collection.immutable.SortedMap

object TransformData {

  def algebra[Data](keepByDefault: Boolean = false)(
    implicit Data: Birecursive.Aux[Data, data.DataF]
  ): Algebra[transform.TransformF, Data => Option[Data]] = {
    case transform.KeepF => Option.apply

    case transform.DropF => _ => Option.empty

    case transform.StructF(transforms) => inp => Data.project(inp) match {
      case data.StructDataF(fields) =>
        val collected = fields.toList.flatMap {
          case (k, v) =>
            val r = transforms.get(k) match {
              case Some(tf) => tf(v)
              case None if keepByDefault => Some(v)
              case _ => None
            }
            r.map((k, _))
        }
        Some(Data.embed(data.StructDataF(SortedMap(collected: _*))))

      case _ => None
    }
  }

  def apply[Transform, Data](transform: Transform, keepByDefault: Boolean = false)(dat: Data)(
    implicit Transform: Recursive.Aux[Transform, TransformF],
    Schema: Birecursive.Aux[Data, DataF]
  ): Option[Data] = {
    val f = Transform.cata(transform)(algebra(keepByDefault))
    f(dat)
  }
}
