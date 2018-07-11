package datum.blue.schema.json

import alleycats.Empty
import datum.blue.attributes
import datum.blue.attributes.Attributes
import io.circe.Decoder.Result

package object checks {
  def optional[Data: Empty]: PartialFunction[(Result[Data], Attributes), Result[Data]] = {
    case (_, attr) if attributes.isOptional(attr) => Right(Empty[Data].empty)
  }
}
