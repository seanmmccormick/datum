package datum.green.libjson.schemas

import datum.green.libjson.attributes.AttributeReadWriter
import datum.green.patterns.schemas._
import qq.droste.{Algebra, scheme}
import ujson.Js
import upickle.default._

trait SchemaReadWriter { self: AttributeReadWriter =>

  val algebra: Algebra[SchemaF, Js.Value] = Algebra {
    case ValueF(tpe, attributes) =>
      Js.Obj(
        "type" -> Type.asString(tpe),
        "attributes" -> write(attributes)
      )
  }

  implicit val scheamReadWrite: ReadWriter[Schema] = upickle.default
    .readwriter[Js.Value]
    .bimap[Schema](schema => {
      val toJson = scheme.cata(algebra)
      toJson(schema)
    }, js => ???)
}
