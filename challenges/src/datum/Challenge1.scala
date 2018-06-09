package datum

import datum.path._
import datum.schema2._

object Challenge1 {

  // from /input.js/~/#each as src select src/name, src/age, src/school_id into /table_1.csv
  // from /input.js/~/#each as src select src/school_name, src/school_id into /table_2.csv

  val inputjs: String =
    """
      |{
      |    [
      |        {"abc",12,"tech", 123},
      |        {"abc",12,"tech1", 345},
      |        {"xyz",12,"tech", 123},
      |        {"xyz",12,"cmu", 576},
      |    ]
      |}
    """.stripMargin


  val tmp: SchemaRepr = datum.schema2.root(array(struct(
    Name("name") -> text,
    Name("age") -> int,
    Name("school_name") -> text,
    Name("school_id") -> int
  )))

  def main(args: String): Unit = {
    println("=== split json into csv === ")
  }
}
