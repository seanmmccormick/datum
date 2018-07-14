package datum.blue.schema

case class Column[A](value: A, header: Option[String] = None)
