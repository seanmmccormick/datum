package datum.algebras

import datum.patterns.{data, schemas}
import datum.patterns.schemas._
import datum.patterns.attributes._
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.Checkers
import datum.patterns.data.{Data, DataF, ObjValue}
import qq.droste.data.Fix

class DefaultsSpec extends WordSpec with Checkers with Matchers {

  "The Defaults algebra" should {
    "foo" in {
      val person: Schema = schemas.obj()(
        "name" -> schemas.value(TextType),
        "foo" -> schemas.value(BooleanType, Defaults.default(property(true))),
        "age" -> schemas.value(IntType, Defaults.default(property(42)))
      )

      val sample: Data = data.obj(
        "name" -> data.text("Bob")
      )

      // Annotates a Schema with a value of type "Data" - this is the default value at that position.
      // The schema itself might be malformed, so this returns Either[Error, AnnotatedSchema].
      val compiled = Defaults.annotate(person)

      // Assume nothing wrong with the default definitions in the schema.
      val annotated = compiled.right.get

      // Given the annotated schema, we can generate a function from Data => Data,
      // applying defaults where input data is missing.
      val applyDefaults: Data => Data = Defaults.makeFn(annotated)

      // And we can use that function on data
      val result = applyDefaults(sample)

      Fix.un[DataF](result) shouldBe a[ObjValue[Data]]

      // downcast type and check values
      val check = Fix.un[DataF](result).asInstanceOf[ObjValue[Data]]

      check.fields.keySet should contain("foo")
      check.fields.keySet should contain("age")
      check.fields("age") shouldBe data.integer(42)
      check.fields("foo") shouldBe data.boolean(true)
    }
  }
}
