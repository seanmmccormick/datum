package datum.red.schema2

sealed trait Description
case object IntD extends Description
case object TextD extends Description
case object NullD extends Description
