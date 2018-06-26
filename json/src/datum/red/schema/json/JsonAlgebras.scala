package datum.red.schema.json

import datum.red.schema._
import datum.red.helpers._
import datum.red.schema2
import ujson.Js

import scala.collection.immutable.SortedMap

class JsonAlgebras {

  // Simple encoding of schemas <-> json
//  val save: Algebra[SchemaF, Js.Value] = {
//    case IntF => Js.Str("int")
//    case TextF => Js.Str("string")
//    case StructF(fields) => Js.Obj.from(fields)
//  }
//
//  val load: Coalgebra[SchemaF, Js.Value] = {
//    case ujson.Js.Str(x) if x == "string" => TextF
//    case ujson.Js.Str(x) if x == "int" => IntF
//    case ujson.Js.Obj(fields) => schema2.StructF(SortedMap(fields.toSeq:_*))
//  }
}
