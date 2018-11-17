package datum.algebras.defaults
import datum.patterns.attributes._
import datum.patterns.data
import datum.patterns.data.{Data, DataF}
import datum.patterns.schemas._

import cats.instances.either._
import qq.droste.data._
import qq.droste.data.prelude._
import qq.droste.{Algebra, AlgebraM, scheme}

object Defaults {
  val key: AttributeKey = "default".asAttributeKey

  def default(attribute: Attribute): (AttributeKey, Attribute) = key -> attribute

  /*
   * This is an Algebra that, given a default-annotated Schema,
   * can generate a function from Data => Data, replacing missing
   * values as defined in the schema with the correct default.
   */
  val withDefaults: Algebra[AttrF[SchemaF, Data, ?], Data => Data] =
    Algebra[AttrF[SchemaF, Data, ?], Data => Data] {

      // handle cases for `ObjF`s that can either themselves be missing, or contain missing fields
      case AttrF(default, ObjF(schemaFields, attributes)) =>
        inp =>
          Fix.un[DataF](inp) match {

            // Handle case for when the obj itself is missing and has a default defined
            case data.EmptyValue if attributes.contains(key) => default

            // Handle case for when the obj is missing, but might have default fields
            case data.EmptyValue =>
              val nonEmptyFields = schemaFields.mapValues(fn => fn(data.empty)).filter {
                case (_, result) if result == data.empty => false
                case _                                   => true
              }
              if (nonEmptyFields.isEmpty) data.empty
              else data.obj(nonEmptyFields)

            // Handle case for possibly missing fields in the object
            case data.ObjValue(dataFields) =>
              val missingFields = schemaFields.filterKeys(k => !dataFields.keySet.contains(k))
              val defaultFields = missingFields.mapValues(fn => fn(data.empty))

              // Return new data.Obj type with possibly new default fields added
              data.obj(dataFields ++ defaultFields)

            // Otherwise return the input value
            case _ => inp
          }

      // Handle case for replacing empty inputs with default value
      case AttrF(default, schema) if schema.attributes.contains(key) =>
        inp =>
          Fix.un[DataF](inp) match {
            case data.EmptyValue => default
            case _               => inp
          }

      // format: off

      // Otherwise, just return the input
      case _ => inp => inp

      // format: on
    }

  // this requires qq.droste.data.prelude._ for the implicit Functor[AttrF] instance
  private val defaultGen = scheme.cata(withDefaults)

  def makeFn(annotated: Attr[SchemaF, Data]): Data => Data = {
    defaultGen(annotated)
  }
}
