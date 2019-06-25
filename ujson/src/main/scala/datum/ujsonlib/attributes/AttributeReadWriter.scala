package datum.ujsonlib.attributes

import datum.patterns.attributes._
import higherkindness.droste._
import upickle.default._

object AttributeReadWriter {

  val algebra: Algebra[AttributesF, ujson.Value] = Algebra {
    case Property(value)     => ujson.Str(value)
    case NumProperty(value)  => ujson.Num(value)
    case BoolProperty(value) => ujson.Bool(value)
    case Label(name, value)  => ujson.Obj("attr" -> "label", name -> value)
    case Collection(vs)      => ujson.Arr(vs)
    case Flag                => ujson.Obj()
  }

  val coalgebra: Coalgebra[AttributesF, ujson.Value] = Coalgebra[AttributesF, ujson.Value] {
    case ujson.Str(value) => Property(value)

    case ujson.Num(value) => NumProperty(value)

    case ujson.Bool(value) => BoolProperty(value)

    case ujson.Arr(value) => Collection(value.toVector)

    case ujson.Obj(fields) if fields("attr").str == "label" =>
      val (name, value) = fields.filterKeys(_ != "attr").head
      Label(name, value)

    case ujson.Obj(fields) if fields.isEmpty => Flag
  }
}

trait AttributeReadWriter {
  import AttributeReadWriter._

  implicit val attrReadWrite: ReadWriter[Attribute] = upickle.default
    .readwriter[ujson.Value]
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
}
