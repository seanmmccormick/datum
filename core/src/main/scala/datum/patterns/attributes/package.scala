package datum.patterns

import cats.data.{Kleisli, Reader, State}
import qq.droste.data.Fix

package object attributes {
  type Attribute = Fix[AttributesF]

  type AttributeMap = Map[AttributeKey, Attribute]

  type Attributed[T] = Reader[AttributeMap, T]
  type Modifiable[T] = State[AttributeMap, T]

  object Attributed {

    @inline
    def apply[A](f: AttributeMap => A): Attributed[A] = Kleisli.apply[cats.Id, AttributeMap, A](f)

    @inline
    def pure[A](a: A): Attributed[A] = Kleisli.pure(a)

    @inline
    def identity[A]: A => Attributed[A] = pure
  }

  implicit class AttributeStringOps(inp: String) {
    def asAttributeKey: AttributeKey = AttributeKey(inp)
  }

  implicit class AttributedAsKleisliOps[A](foo: A => Attributed[A]) {
    @inline
    def composeWith(rhs: A => Attributed[A]): A => Attributed[A] = (Kleisli(foo) compose Kleisli(rhs)).run

    // Kleisli composition (aka "fish" operator) - this might find its way into cats eventually
    @inline
    def >=>(rhs: A => Attributed[A]): A => Attributed[A] = composeWith(rhs)
  }

  def property(flag: Boolean): Attribute = Fix.apply[AttributesF](BooleanPropertyF(flag))

  def property(text: String): Attribute = Fix.apply[AttributesF](TextPropertyF(text))

  def and(left: Attribute, right: Attribute): Attribute = Fix.apply[AttributesF](AndF(left, right))

  def or(left: Attribute, right: Attribute): Attribute = Fix.apply[AttributesF](OrF(left, right))

  def label(name: String, attr: Attribute): Attribute = Fix.apply[AttributesF](LabelF(name, attr))
}
