package datum.avrolib.data.errors

case class InvalidRecordOnWrite(msg: String) extends Exception

case class InvalidRecordOnRead(msg: String) extends Exception
