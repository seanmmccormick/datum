package datum.algebras.defaults
import datum.algebras.defaults.Defaults.key
import datum.patterns.data
import datum.patterns.data.Data
import datum.patterns.schemas._

import cats.instances.either._
import qq.droste.{AlgebraM, scheme}
import qq.droste.data.Attr

/*
 * Given a Schema with "default" attributes, this:
 *  1) Validates that the default values correspond to the schema correctly
 *  2) Generates the default data value from the attribute value
 *  3) Returns the annotated SchemaF tree with default values attached
 */
class CompileDefaults(rules: AttributeCompilationRules) {

  // Todo - turn String into a better error type
  val compileAlgebra: AlgebraM[Either[String, ?], SchemaF, Attr[SchemaF, Data]] = {

    // This function helps Scala infer types
    def using(
      compiled: Either[String, Data],
      schema: SchemaF[Attr[SchemaF, Data]]
    ): Either[String, Attr[SchemaF, Data]] = {
      compiled.map { default =>
        Attr.apply(default, schema)
      }
    }

    AlgebraM[Either[String, ?], SchemaF, Attr[SchemaF, Data]] {
      case v @ ValueF(IntType, attrs) if attrs.contains(key)     => using(rules.integer(attrs(key)), v)
      case v @ ValueF(TextType, attrs) if attrs.contains(key)    => using(rules.text(attrs(key)), v)
      case v @ ValueF(BooleanType, attrs) if attrs.contains(key) => using(rules.boolean(attrs(key)), v)

      // An error if a schema node has a default, but no rules match
      case otherwise if otherwise.attributes.contains(key) => Left("Invalid Default!")

      // the default is "data.empty" if there is no default attribute defined
      case otherwise => using(Right(data.empty), otherwise)
    }
  }

  // this requires cats.instance.either._ for the Monad instance of Either
  private val generator = scheme.cataM(compileAlgebra)

  def compile(schema: Schema): Either[String, Attr[SchemaF, Data]] = {
    generator(schema)
  }
}

object CompileDefaults {
  def apply(rules: AttributeCompilationRules): CompileDefaults = new CompileDefaults(rules)
}
