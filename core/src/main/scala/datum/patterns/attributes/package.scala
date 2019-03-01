package datum.patterns

import cats.data.{Kleisli, Reader, State}
import qq.droste.data.Fix

package object attributes {
  type Attribute = Fix[AttributesF]

  type AttributeMap = Map[String, Attribute]

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

  implicit class AttributedAsKleisliOps[A](foo: A => Attributed[A]) {

    @inline
    def composeWith(rhs: A => Attributed[A]): A => Attributed[A] = (Kleisli(foo) compose Kleisli(rhs)).run

    // Kleisli composition (aka "fish" operator) - this might find its way into cats eventually
    @inline
    def >=>(rhs: A => Attributed[A]): A => Attributed[A] = composeWith(rhs)
  }

  def property(flag: Boolean): Attribute = Fix.apply[AttributesF](BoolProperty(flag))

  def property(text: String): Attribute = Fix.apply[AttributesF](Property(text))

  def property(value: Double): Attribute = Fix.apply[AttributesF](NumProperty(value))

  def and(left: Attribute, right: Attribute): Attribute = Fix.apply[AttributesF](And(left, right))

  def or(left: Attribute, right: Attribute): Attribute = Fix.apply[AttributesF](Or(left, right))

  def label(name: String, attr: Attribute): Attribute = Fix.apply[AttributesF](Label(name, attr))

  def collection(values: Attribute*): Attribute = Fix.apply[AttributesF](Collection(values.toVector))

  //Some implicit conversions for building attributes
  implicit def textProp(v: String): Attribute = property(v)
  implicit def numProp(v: Double): Attribute = property(v)
  implicit def boolProp(v: Boolean): Attribute = property(v)
  implicit def liftPair[T](x: (String, T))(implicit fn: T => Attribute): (String, Attribute) = (x._1, fn(x._2))

}
