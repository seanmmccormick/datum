package datum.blue

package object attributes {

  type Attributes = Map[AttributeKey, AttributeValue]

  def isOptional(attrs: Map[AttributeKey, AttributeValue]): Boolean = {
    attrs.contains(Optional) && (attrs(Optional) match {
      case Flag(true) => true
      case _          => false
    })
  }
}
