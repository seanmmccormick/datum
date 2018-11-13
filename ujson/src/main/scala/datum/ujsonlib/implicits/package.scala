package datum.ujsonlib
import datum.ujsonlib.attributes.AttributeReadWriter
import datum.ujsonlib.schemas.SchemaReadWriter

//trait Overrides { self: AttributeReadWriter =>
//  import upickle.default._
//
//  implicit lazy val mapAsObj: ReadWriter[Map[AttributeKey, Attribute]] = upickle.default
//    .readwriter[Map[String, Attribute]]
//    .bimap(x => {
//      x.map { case (key, value) => (key.key, value) }
//    }, y => y.map { case (key, value) => (AttributeKey(key), value) })
//}

package object implicits extends SchemaReadWriter with AttributeReadWriter
