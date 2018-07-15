package datum.blue.transform.json

import datum.blue.transform.TransformF
import datum.blue.transform
import datum.red.helpers.Algebra
import io.circe.{Encoder, Json}
import turtles.Recursive

object Writer {

  val algebra: Algebra[TransformF, Json] = {
    case transform.KeepF => Json.fromString("keep")

    case transform.DropF => Json.fromString("drop")

    case transform.RenameF(to, trg) =>
      Json.obj(
        "rename" -> Json.obj(
          "to" -> Json.fromString(to),
          "target" -> trg
        ))

    case transform.SelectFieldF(field, target) =>
      Json.obj(
        "select" -> Json.obj(
          "field" -> Json.fromString(field),
          "target" -> target
        ))

    case transform.ExplodeF(target) => Json.obj("explode" -> target)

    case transform.StructF(fields) => Json.obj("struct" -> Json.fromFields(fields))
  }

  implicit def encode[R](implicit R: Recursive.Aux[R, TransformF]): Encoder[R] = new Encoder[R] {
    override def apply(a: R): Json = R.cata(a)(algebra)
  }
}
