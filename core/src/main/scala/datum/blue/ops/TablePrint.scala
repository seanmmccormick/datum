package datum.blue.ops

import datum.blue.data._
import datum.blue.schema
import datum.blue.schema.SchemaF
import turtles.{Algebra, Recursive}

object TablePrint {

  object Tabulator {

    def format(table: Seq[Seq[String]]): String = table match {
      case Seq() => ""
      case _ =>
        val sizes = for (row <- table) yield for (cell <- row) yield if (cell == null) 0 else cell.toString.length
        val colSizes = for (col <- sizes.transpose) yield col.max
        val rows = for (row <- table) yield formatRow(row, colSizes)
        formatRows(rowSeparator(colSizes), rows)
    }

    private def formatRows(rowSeparator: String, rows: Seq[String]): String =
      (rowSeparator ::
        rows.head ::
        rowSeparator ::
        rows.tail.toList :::
        rowSeparator ::
        List()).mkString("\n")

    private def formatRow(row: Seq[Any], colSizes: Seq[Int]) = {
      val cells = for ((item, size) <- row.zip(colSizes)) yield if (size == 0) "" else ("%" + size + "s").format(item)
      cells.mkString("|", "|", "|")
    }

    private def rowSeparator(colSizes: Seq[Int]) = colSizes map { "-" * _ } mkString ("+", "+", "+")
  }

  val algebra: Algebra[DataF, Vector[String]] = {
    case TextDataF(v)    => Vector(v)
    case IntegerDataF(v) => Vector(v.toString)
    case BooleanDataF(v) => Vector(v.toString)
    case RealDataF(d)    => Vector(f"$d%.3f")
    case StructDataF(v)  => Vector("{ " + v.map { case (k, v) => s"\"$k\": \"$v\""}.mkString(", ") + "}")
    case RowDataF(cols)  => cols.flatten
  }

  val headerAlg: Algebra[SchemaF, Vector[String]] = {
    case schema.RowF(elems, _) =>
      elems.zipWithIndex.map {
        case (col, idx) =>
          col.header.getOrElse(s"Column $idx")
      }
    case _ => Vector.empty
  }

  def apply[Schema, Data](sch: Schema, inp: List[Data])(implicit Data: Recursive.Aux[Data, DataF],
                                                        Schema: Recursive.Aux[Schema, SchemaF]): String = {
    val header = Schema.cata(sch)(headerAlg)
    val collected = inp.map { d =>
      Data.cata(d)(algebra(_ => Vector.empty))
    }

    Tabulator.format(header :: collected)
  }
}
