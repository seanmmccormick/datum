package datum.green.patterns.schemas

case class Column[A](value: A, header: Option[String] = None)