package datum.avrolib
import com.fasterxml.jackson.databind.JsonNode
import datum.patterns.properties._
import higherkindness.droste.scheme

package object properties {

  private val writeAvroFn: Property => JsonNode = scheme.cata(PropertiesReadWriter.algebra)

  private val readAvroFn: JsonNode => Property = scheme.ana(PropertiesReadWriter.coalgebra)

  implicit class PropertyToAvroOps(p: Property) {
    def toAvro: JsonNode = writeAvroFn(p)
  }

  implicit class AvroToPropertyOps(n: JsonNode) {
    def toDatum: Property = readAvroFn(n)
  }
}
