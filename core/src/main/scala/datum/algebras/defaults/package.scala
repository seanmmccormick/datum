package datum.algebras
import datum.patterns.attributes._

package object defaults {
  val key: AttributeKey = "default".asAttributeKey

  def use(attribute: Attribute): (AttributeKey, Attribute) = key -> attribute
}
