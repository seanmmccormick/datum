package datum.blue.data.json

import datum.blue.data._
import io.circe._
import turtles.{Algebra, Recursive}

object Writer {

  val algebra: Algebra[DataF, Json] = {
    case StructDataF(fields) => Json.fromFields(fields)
    case RowDataF(values)    => Json.fromValues(values)
    case IntegerDataF(v)     => Json.fromLong(v)
    case RealDataF(v)        => Json.fromDoubleOrNull(v)
    case TextDataF(v)        => Json.fromString(v)
    case BooleanDataF(v)     => Json.fromBoolean(v)
    case NullDataF           => Json.Null
  }

  implicit def encode[R](implicit R: Recursive.Aux[R, DataF]): Encoder[R] = new Encoder[R] {
    override def apply(a: R): Json = R.cata(a)(algebra)
  }
}
