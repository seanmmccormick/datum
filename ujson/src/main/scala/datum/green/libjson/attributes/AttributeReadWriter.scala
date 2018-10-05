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
}

trait AttributeReadWriter {
  import AttributeReadWriter._

  implicit val attrReadWrite: ReadWriter[Attribute] = upickle.default
    .readwriter[Js.Value]
    .bimap[Attribute](
    attr => {
      val toJson = scheme.cata(algebra)
      toJson(attr)
    },
    js => ???
  )

  implicit val attrKeyReadWrite: ReadWriter[AttributeKey] = ???
}
