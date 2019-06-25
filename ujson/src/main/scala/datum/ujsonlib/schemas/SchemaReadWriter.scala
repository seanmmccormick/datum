package datum.ujsonlib.schemas

import datum.ujsonlib.attributes.AttributeReadWriter
import datum.patterns.attributes.Attribute
import datum.patterns.schemas._
import higherkindness.droste.{Algebra, Coalgebra, scheme}
import upickle.default._

import scala.collection.immutable.SortedMap

trait SchemaReadWriter { self: AttributeReadWriter =>

  val algebra: Algebra[SchemaF, ujson.Value] = Algebra {
    case ObjF(fields, attributes) =>
      ujson.Obj(
        "fields" -> fields,
        "attributes" -> writeJs(attributes)
      )

    case RowF(columns, attributes) =>
      ujson.Obj(
        "columns" ->
          columns.map(
            col =>
              col.header.map { hdr =>
                ujson.Obj("schema" -> col.value, "header" -> hdr)
              } getOrElse {
                ujson.Obj("schema" -> col.value)
            }
          ),
        "attributes" -> writeJs(attributes)
      )

    case ArrayF(element, attributes) =>
      ujson.Obj(
        "array" -> element,
        "attributes" -> writeJs(attributes)
      )

    case NamedUnionF(alternatives, attributes) =>
      ujson.Obj(
        "union" -> alternatives,
        "attributes" -> writeJs(attributes)
      )

    case IndexedUnionF(alternatives, attributes) =>
      ujson.Obj(
        "indexed" -> alternatives,
        "attributes" -> writeJs(attributes)
      )

    case ValueF(tpe, attributes) =>
      ujson.Obj(
        "type" -> Type.asString(tpe),
        "attributes" -> writeJs(attributes)
      )
  }

  val coalgebra: Coalgebra[SchemaF, ujson.Value] = Coalgebra[SchemaF, ujson.Value] {
    case ujson.Obj(fields) if fields.contains("type") =>
      val attrs = read[Map[String, Attribute]](fields("attributes"))
      ValueF(Type.fromString(fields("type").str).get, attrs)

    case ujson.Obj(fields) if fields.contains("columns") =>
      val attrs = read[Map[String, Attribute]](fields("attributes"))
      val elements = fields("columns").arr.view.map { colJs =>
        val header = colJs.obj.get("header").map(_.str)
        Column[ujson.Value](colJs("schema"), header)
      }.toVector
      RowF(elements, attrs)

    case ujson.Obj(fields) if fields.contains("array") =>
      val attrs = read[Map[String, Attribute]](fields("attributes"))
      ArrayF(fields("array"), attrs)

    case ujson.Obj(fields) if fields.contains("fields") =>
      val attrs = read[Map[String, Attribute]](fields("attributes"))
      ObjF(SortedMap(fields("fields").obj.toSeq: _*), attrs)

    case ujson.Obj(fields) if fields.contains("union") =>
      val attrs = read[Map[String, Attribute]](fields("attributes"))
      NamedUnionF(SortedMap(fields("union").obj.toSeq: _*), attrs)

    case ujson.Obj(fields) if fields.contains("indexed") =>
      val attrs = read[Map[String, Attribute]](fields("attributes"))
      IndexedUnionF(fields("indexed").arr.to[Vector], attrs)

    case fuuu =>
      println("============ FUUUUUUU =========")
      pprint.pprintln(fuuu)
      ???
  }

  implicit val scheamReadWrite: ReadWriter[Schema] = upickle.default
    .readwriter[ujson.Value]
    .bimap[Schema](schema => {
      val toJsonFn = scheme.cata(algebra)
      toJsonFn(schema)
    }, js => {
      val fromJsFn = scheme.ana(coalgebra)
      fromJsFn(js)
    })
}
