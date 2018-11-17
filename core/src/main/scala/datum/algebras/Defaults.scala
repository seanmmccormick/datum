package datum.algebras
import datum.patterns.attributes._
import datum.patterns.data
import datum.patterns.data.{Data, DataF}
import datum.patterns.schemas._
import qq.droste.{Algebra, AlgebraM, scheme}
import qq.droste.data._
import qq.droste.data.prelude._
import cats.instances.either._

object Defaults {
  val key: AttributeKey = "default".asAttributeKey

  def default(attribute: Attribute): (AttributeKey, Attribute) = key -> attribute

  /*
   * Given a Schema with "default" attributes, this:
   *  1) Validates that the default values correspond to the schema correctly
   *  2) Generates the default data value from the attribute value
   *  3) Returns the annotated SchemaF tree with default values attached
   */
  // Todo - turn String into a better error type
  val compile: AlgebraM[Either[String, ?], SchemaF, Attr[SchemaF, Data]] = {

    // This function helps Scala infer types
    def using(default: Data, schema: SchemaF[Attr[SchemaF, Data]]): Either[String, Attr[SchemaF, Data]] = {
      Right(Attr.apply(default, schema))
    }

    AlgebraM[Either[String, ?], SchemaF, Attr[SchemaF, Data]] {

      // todo
      case v @ ValueF(BooleanType, attrs) if attrs.contains(key) =>
        using(data.boolean(true), v)

      // the default is "data.empty" if there is no default attribute defined
      case otherwise => using(data.empty, otherwise)
    }
  }

  def annotate(schema: Schema): Either[String, Attr[SchemaF, Data]] = {
    val gen = scheme.cataM(compile)
    gen(schema)
  }

  /*
   * This is an Algebra that, given a default-annotated Schema,
   * can generate a function from Data => Data, replacing missing
   * values as defined in the schema with the correct default.
   */
  val withDefaults: Algebra[AttrF[SchemaF, Data, ?], Data => Data] =
    Algebra[AttrF[SchemaF, Data, ?], Data => Data] {

      // handle case for objects that can either themselves be missing, or contain missing fields
      case AttrF(default, ObjF(schemaFields, attributes)) =>
        inp =>
          Fix.un[DataF](inp) match {

            // Handle case for when the obj itself is missing
            case data.EmptyValue if attributes.contains(key) => default

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

      // Otherwise, just return the input
      case _ => inp => inp
    }

  // this requires qq.droste.data.prelude._ for the implicit Functor[AttrF] instance
  private val defaultGen = scheme.cata(withDefaults)

  def makeFn(annotated: Attr[SchemaF, Data]): Data => Data = {
    defaultGen(annotated)
  }
}
