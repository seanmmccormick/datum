package datum.green.libjson.attributes

import datum.green.patterns.attributes._
import qq.droste._
import ujson.Js
import upickle.default._

object AttributeReadWriter {

  val algebra: Algebra[AttributesF, Js.Value] = Algebra {
    case TextPropertyF(value)    => Js.Str(value)
    case NumericPropertyF(value) => Js.Num(value)
    case BooleanPropertyF(value) => Js.Bool(value)
    case LabelF(name, value)     => Js.Obj("attr" -> "label", name -> value)
    case AndF(lhs, rhs)          => Js.Obj("attr" -> "and", "left" -> lhs, "right" -> rhs)
    case OrF(lhs, rhs)           => Js.Obj("attr" -> "or", "left" -> lhs, "right" -> rhs)
  }

  val coalgebra: Coalgebra[AttributesF, Js.Value] = Coalgebra[AttributesF, Js.Value] {
    case Js.Str(value) => TextPropertyF(value)

    case Js.Num(value) => NumericPropertyF(value)

    case Js.Bool(value) => BooleanPropertyF(value)

    case Js.Obj(fields) if fields("attr").str == "label" =>
      val (name, value) = fields.filterKeys(_ != "attr").head
      LabelF(name, value)

    case Js.Obj(fields) if fields("attr").str == "and" => AndF(fields("left"), fields("right"))

    case Js.Obj(fields) if fields("attr").str == "or" => OrF(fields("left"), fields("right"))
  }
}

trait AttributeReadWriter {
  import AttributeReadWriter._

  implicit val attrReadWrite: ReadWriter[Attribute] = upickle.default
    .readwriter[Js.Value]
    .bimap[Attribute](
      attr => {
        val toJsonFn = scheme.cata(algebra)
        toJsonFn(attr)
      },
      js => {
        val fromJsonFn = scheme.ana(coalgebra)
        fromJsonFn(js)
      }
    )

  implicit val attrKeyReadWrite: ReadWriter[AttributeKey] = upickle.default
    .readwriter[Js.Value]
    .bimap[AttributeKey](
      ak => ak.key, {
        case Js.Str(x) => AttributeKey(x)
      }
    )
}
