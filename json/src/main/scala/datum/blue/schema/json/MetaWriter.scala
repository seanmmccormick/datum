package datum.blue.schema.json

import datum.blue.meta
import datum.blue.meta._
import io.circe._
import turtles.{Algebra, Recursive}

object MetaWriter {

  val algebra: Algebra[MetaF, Json] = {
    case BooleanPropertyF(x) => Json.fromBoolean(x)
    case NumericPropertyF(x) => Json.fromDoubleOrNull(x)
    case TextPropertyF(x)    => Json.fromString(x)
    case LabelF(name, x)     => Json.obj("label" -> Json.fromString(name), "value" -> x)
    case OrF(lhs, rhs)       => Json.obj("op" -> Json.fromString("or"), "lhs" -> lhs, "rhs" -> rhs)
    case AndF(lhs, rhs)      => Json.obj("op" -> Json.fromString("and"), "lhs" -> lhs, "rhs" -> rhs)
  }

  implicit def encoder[R](implicit Meta: Recursive.Aux[R, MetaF]): Encoder[R] = new Encoder[R] {
    override def apply(a: R): Json = Meta.cata(a)(algebra)
  }
}
