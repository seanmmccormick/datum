package datum.blue

import datum.blue.schema.SchemaF
import turtles.Corecursive

import scala.collection.immutable.SortedMap

package object data {

  def struct[R](
      fields: SortedMap[String, R]
  )(implicit R: Corecursive.Aux[R, DataF]): R = {
    R.embed(StructDataF(fields))
  }

  def text[R](value: String)(implicit R: Corecursive.Aux[R, DataF]): R = {
    R.embed(TextDataF(value))
  }

  def real[R](value: Double)(implicit R: Corecursive.Aux[R, DataF]): R = {
    R.embed(RealDataF(value))
  }

  def integer[R](value: Long)(implicit R: Corecursive.Aux[R, DataF]): R = {
    R.embed(IntegerDataF(value))
  }
}
