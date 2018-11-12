package datum.libjson.schemas

import datum.libjson.attributes.AttributeReadWriter
import datum.patterns.attributes.{Attribute, AttributeKey}
import datum.patterns.schemas._
import qq.droste.{Algebra, Coalgebra, scheme}
import ujson.Js
import upickle.default._

import scala.collection.immutable.SortedMap

trait SchemaReadWriter { self: AttributeReadWriter =>

  val algebra: Algebra[SchemaF, Js.Value] = Algebra {
    case StructF(fields, attributes) =>
      Js.Obj(
        "fields" -> fields,
        "attributes" -> writeJs(attributes)
      )

    case RowF(columns, attributes) =>
      Js.Obj(
        "columns" ->
          columns.map(
            col =>
              col.header.map { hdr =>
                Js.Obj("schema" -> col.value, "header" -> hdr)
              } getOrElse {
                Js.Obj("schema" -> col.value)
            }
          ),
        "attributes" -> writeJs(attributes)
      )

    case ValueF(tpe, attributes) =>
      Js.Obj(
        "type" -> Type.asString(tpe),
        "attributes" -> writeJs(attributes)
      )
  }

  val coalgebra: Coalgebra[SchemaF, Js.Value] = Coalgebra[SchemaF, Js.Value] {
    case Js.Obj(fields) if fields.contains("type") =>
      val attrs = readJs[Map[AttributeKey, Attribute]](fields("attributes"))
      ValueF(Type.fromString(fields("type").str).get, attrs)

    case Js.Obj(fields) if fields.contains("columns") =>
      val attrs = readJs[Map[AttributeKey, Attribute]](fields("attributes"))
      val elements = fields("columns").arr.view.map { colJs =>
        val header = colJs.obj.get("header").map(_.str)
        Column[Js.Value](colJs("schema"), header)
      }.toVector
      RowF(elements, attrs)

    case Js.Obj(fields) if fields.contains("fields") =>
      val attrs = readJs[Map[AttributeKey, Attribute]](fields("attributes"))
      StructF(SortedMap(fields("fields").obj.toSeq: _*), attrs)
  }

  implicit val scheamReadWrite: ReadWriter[Schema] = upickle.default
    .readwriter[Js.Value]
    .bimap[Schema](schema => {
      val toJsonFn = scheme.cata(algebra)
      toJsonFn(schema)
    }, js => {
      val fromJsFn = scheme.ana(coalgebra)
      fromJsFn(js)
    })
}
