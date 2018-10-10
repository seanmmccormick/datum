package datum.green.libjson
import datum.green.libjson.attributes.AttributeReadWriter
import datum.green.libjson.schemas.SchemaReadWriter
import datum.green.patterns.attributes.{Attribute, AttributeKey}

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
