package datum.modifiers

import datum.patterns.attributes._

object Optional {
  val key: String = "optional"
  val enable: (String, Attribute) = key -> property(true)
}
