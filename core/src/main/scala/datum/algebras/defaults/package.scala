package datum.algebras
import datum.patterns.attributes._

package object defaults {
  val key: String = "default"

  def use(attribute: Attribute): (String, Attribute) = key -> attribute
}
