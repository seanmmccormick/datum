package datum.algebras

import datum.algebras.defaults.{CompileDefaults, DefaultCompilationRules, Defaults}
import datum.patterns.{data, schemas}
import datum.patterns.schemas._
import datum.patterns.attributes._
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.Checkers
import datum.patterns.data.{Data, DataF, ObjValue}
import qq.droste.data.Fix

class DefaultsSpec extends WordSpec with Checkers with Matchers {

  // Constructs a "schema defaults compiler/interpreter" that has rules for mapping
  // Attributes into Data
  val defaults = CompileDefaults(DefaultCompilationRules)

  "The Defaults algebra" should {
    "work given a basic example" in {
      val person: Schema = schemas.obj()(
        "name" -> schemas.value(TextType),
        "foo" -> schemas.value(BooleanType, Defaults.default(property(true))),
        "age" -> schemas.value(IntType, Defaults.default(property(42)))
      )

      val sample: Data = data.obj(
        "name" -> data.text("Bob")
      )

      // Annotates a Schema with "Data" values - this is the default value at that position.
      // The schema itself might be malformed, so this returns Either[Error, AnnotatedSchema].
      val compiled = defaults.compile(person)

      // Assume nothing wrong with the default definitions in the schema.
      val annotated = compiled.right.get

      // Given the annotated schema, we can generate a function from Data => Data,
      // applying defaults where input data is missing.
      val applyDefaults: Data => Data = Defaults.makeFn(annotated)

      // And we can use that function on data
      val result = applyDefaults(sample)

      Fix.un[DataF](result) shouldBe a[ObjValue[_]]

      // downcast type and check values
      val check = Fix.un[DataF](result).asInstanceOf[ObjValue[Data]]

      check.fields.keySet should contain("foo")
      check.fields.keySet should contain("age")
      check.fields("age") shouldBe data.integer(42)
      check.fields("foo") shouldBe data.boolean(true)
    }

    "insert fields given an empty input data obj" in {
      val schema: Schema = schemas.obj()(
        "foo" -> schemas.value(TextType, Defaults.default(property("hello"))),
        "bar" -> schemas.value(IntType, Defaults.default(property(1))),
        "nested" -> schemas.obj()(
          "inner" -> schemas.value(BooleanType, Defaults.default(property(false)))
        ),
        "nope" -> schemas.obj()("missing" -> schemas.value(TextType))
      )
      val annotated = defaults.compile(schema).right.get
      val fn = Defaults.makeFn(annotated)

      val result = fn(data.empty)
      Fix.un[DataF](result) shouldBe a[ObjValue[_]]

      // downcast type and check values
      val check = Fix.un[DataF](result).asInstanceOf[ObjValue[Data]]

      check.fields.keySet should contain("foo")
      check.fields.keySet should contain("bar")
      check.fields.keySet should contain("nested")
      check.fields.keySet shouldNot contain("nope")

      check.fields("foo") shouldBe data.text("hello")
      check.fields("bar") shouldBe data.integer(1)
      check.fields("nested") shouldBe data.obj("inner" -> data.boolean(false))

    }
  }
}
