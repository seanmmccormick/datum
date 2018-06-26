package datum.red

import datum.red.schema2.StructF
import schemes.Fix

import scala.collection.immutable.SortedMap

package object schema {

  // Schema description language
  type Schema = Fix[SchemaF]

  def int: Schema = fix(IntF)

  def text: Schema = fix(TextF)

  def struct(fields: (String, Schema)*): Schema = {
    fix(StructF(SortedMap(fields:_*)))
  }

  // help out scala compiler..
  private def fix[A <: SchemaF[Schema]](a: A): Schema = Fix(a)


  // Path selection language
  type Path = Fix[PathF]

  def select(part: String, path: Path): Path = Fix.apply[PathF](StructSelectF(part, path))

  val asInt: Path = Fix.apply[PathF](IntSelect)

  val asText: Path = Fix.apply[PathF](TextSelect)

  implicit class PathOps(val inp: Path) extends AnyVal {
    def ::(part: String): Path = select(part, inp)
  }
}
