package datum.algebras

import datum.modifiers.Optional
import datum.patterns.data.Data
import datum.patterns.schemas
import datum.patterns.schemas.{IntType, Schema, TextType}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.WordSpec
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers

class CorrespondsSpec extends WordSpec with Checkers {

  val test: Schema = schemas.obj()(
    "foo" -> schemas.value(IntType, Optional.key -> true),
    "bar" -> schemas.value(TextType)
  )

  val other: Schema = schemas.obj()(
    "no" -> schemas.value(IntType)
  )


}
