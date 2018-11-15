package datum.algebras

import cats.data.Reader
import datum.modifiers.Optional
import datum.patterns.attributes.Attributed
import datum.{DataGen, SchemaGen}
import datum.patterns.schemas.{IntType, Schema, TextType}
import datum.patterns.data.Data
import datum.patterns.{attributes, data, schemas}
import org.scalatest.prop.Checkers
import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}

class CorrespondsSpec extends WordSpec with Checkers with Matchers {

  val genDataFromSchema: Gen[(Schema, List[Data])] = {
    for {
      s <- SchemaGen.genStruct()
      d <- DataGen.generatorFor(s)
      ds <- Gen.listOf(d)
    } yield (s, ds)
  }

  implicit val arb: Arbitrary[(Schema, List[Data])] = Arbitrary(genDataFromSchema)

  import cats.data.Kleisli
  import cats.implicits._
  import cats.instances._

  val wat: Boolean => Attributed[Boolean] = { x =>
    Kleisli.pure(x)
  }

  val foo: Boolean => Attributed[Boolean] = { x =>
    Kleisli.pure(x)
  }

  val opt: Boolean => Attributed[Boolean] = {
    case true => Kleisli.pure(true)
    case false =>
      Reader {
        case attrs if attrs.contains(Optional.key) =>
          true
        case attrs                                 =>
          if(attrs.nonEmpty) println(attrs)
          false
      }
  }

  val stab = Kleisli(wat) compose Kleisli(foo) compose Kleisli(opt)

  val correspondsFn = new Corresponds(stab.run)

  val otherSchema: Schema = schemas.obj()(
    "foo" -> schemas.value(IntType),
    "bar" -> schemas.value(TextType)
  )

  val negFn: Data => Boolean = correspondsFn.makeFn(otherSchema)

  "The correspondence function" should {
    "work for schema generated data" in {
      check {
        forAll { generated: (Schema, List[Data]) =>
          val (schema, data) = generated
          val fn: Data => Boolean = correspondsFn.makeFn(schema)
          data.forall(fn) && (data.isEmpty || !data.forall(negFn))
        }
      }
    }

    "work with optional value" in {
      val optional = Map(Optional.key -> attributes.property(true))

      val opt = schemas.obj()(
        "required" -> schemas.value(IntType),
        "maybe" -> schemas.value(IntType, optional)
      )

      val checkFn = correspondsFn.makeFn(opt)

      val d1 = data.struct(
        "required" -> data.integer(1),
        "maybe" -> data.integer(1)
      )

      val d2 = data.struct(
        "required" -> data.integer(2),
        "maybe" -> data.empty
      )

      val d3 = data.struct(
        "required" -> data.integer(3)
      )

      val d4 = data.struct("nope" -> data.integer(4))

      checkFn(d1) shouldBe true
      checkFn(d2) shouldBe true
      checkFn(d3) shouldBe true
      checkFn(d4) shouldBe false
    }
  }
}
