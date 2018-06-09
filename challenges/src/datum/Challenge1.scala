package datum

import dataum.data.{IntValue, NoValue, TextValue, Value}
import datum.data.{Data, LocatedData}
import datum.helpers.Algebra
import datum.located
import datum.path._
import datum.schema2._
import schemes.Schemes

object Challenge1 {

  // from /input.js/~/#each as src select src/name, src/age, src/school_id into /table_1.csv
  // from /input.js/~/#each as src select src/school_name, src/school_id into /table_2.csv

  val inputjs: String =
    """
       |[
       |    [
       |        ["tony",1, 40, "mit", 123],
       |        ["thor",2, 40, "au", 345],
       |        ["bruce",3, 35, "mit", 123],
       |        ["steve",4, 100, "suny", 576]
       |    ]
       |]
    """.stripMargin


  val tmp: SchemaRepr = datum.schema2.root(array(struct(
    Name("name") -> text,
    Name("age") -> int,
    Name("school_name") -> text,
    Name("school_id") -> int
  )))

  val blah = ujson.read(inputjs).arr(0).arr
  println(blah)

  val csvAlg: Algebra[located.LocatedF[Value, ?], List[String]] = {
    case located.ArrayF(x) => x.map(_.mkString("|"))
    case located.RootF(r) => r
    case located.StructF(x) => x.valuesIterator.toList.map(_.mkString("|"))
    case located.EntryF(v) => v match {
      case TextValue(t) => List(t)
      case IntValue(i) => List(i.toString)
      case NoValue => List("?")
    }
  }

  def save(d: LocatedData): Unit = {
    val csv = Schemes.cata[located.LocatedF[Value, ?], List[String]](d)(csvAlg)
    println(csv.mkString(","))
  }
  
  def main(args: Array[String]): Unit = {
    println("=== split json into csv === ")
    path.named("name", path.end)
    path.named("age", path.end)
  }
}
