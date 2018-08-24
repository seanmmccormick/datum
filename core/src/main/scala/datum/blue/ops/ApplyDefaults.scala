package datum.blue.ops

import cats.arrow.FunctionK
import cats.data.State
import cats.free.Cofree
import cats.{Eval, Now, ~>}
import datum.blue.attributes._
import datum.blue.data._
import datum.blue.schema._
import turtles.AlgebraM
import turtles.data.Fix
//import turtles.instances.fixedpoint.Cofree
import turtles.{Algebra, Birecursive, Corecursive, GAlgebra, Recursive}

import scala.collection.immutable.SortedMap

class ApplyDefaults(wat: Map[datum.blue.schema.Type, Attr => Option[DataF[Nothing]]]) {

  private def attrAsData[F[_[_]]](implicit Data: Corecursive.Aux[F[DataF], DataF]): Algebra[AttrF, Option[F[DataF]]] = {
    case BooleanPropertyF(v) => Some(Data.embed(BooleanDataF(v)))
    case _                   => None

  }

  def validate[F[_[_]]]()(implicit Data: Corecursive.Aux[F[DataF], DataF]): SchemaF ~> SchemaF =
    new ~>[SchemaF, SchemaF] {
      override def apply[A](fa: SchemaF[A]): SchemaF[A] = fa match {

        case value @ ValueF(BooleanType, attrs) if attrs.contains(common.default) =>
          val zz = wat.get(BooleanType).flatMap(fn => fn(attrs(common.default)))

          ???
      }
    }
}

class AttrToDataAttempt1(baseTypes: Map[datum.blue.schema.Type, Attr => Option[DataF[Nothing]]]) {
  import turtles.implicits._

  def interesting[F[_[_]]](implicit D: Birecursive.Aux[F[DataF], DataF],
                           A: Recursive.Aux[F[AttrF], AttrF]): GAlgebra[(F[AttrF], ?), AttrF, F[DataF]] = {

    case BooleanPropertyF(v) => D.embed(BooleanDataF(v))

    case NumericPropertyF(v) => D.embed(RealDataF(v))

    case TextPropertyF(v) => D.embed(TextDataF(v))

    case LabelF(label, (_, q)) =>
      D.embed(StructDataF(SortedMap(label -> q)))

    case AndF(lhs, rhs) =>
      (A.project(lhs._1), A.project(rhs._1)) match {
        // format: off
        case   (LabelF(_, _), LabelF(_, _))
             | (LabelF(_, _), AndF(_, _))
             | (AndF(_, _), LabelF(_, _))
             | (AndF(_, _), AndF(_, _)) =>
        // format: on
        val f1 = D.project(lhs._2).asInstanceOf[StructDataF[F[DataF]]].fields
        val f2 = D.project(lhs._2).asInstanceOf[StructDataF[F[DataF]]].fields
        D.embed(StructDataF(f1 ++ f2))
      }
  }

//  def omg[F[_[_]]]()(implicit Data: Birecursive.Aux[F[DataF], DataF], A: Recursive.Aux[F[AttrF], AttrF]) = {
//    val blah: Attr = ???
//    blah.para(interesting)
//  }
}

import cats.syntax.either._

trait DefaultBoolean {
  import DefaultRules._

  def boolean[F[_[_]]](attr: Attr)(
    implicit D: Corecursive.Aux[F[DataF], DataF]): Either[InvalidDefaultAttributeValue, F[DataF]] =
    attr.unFix match {
      case BooleanPropertyF(v) => Right(D.embed(BooleanDataF(v)))
      case _                   => Left(InvalidDefaultAttributeValue("failed to parse default for boolean"))
    }
}

trait DefaultInteger {
  import DefaultRules._

  def integer[F[_[_]]](attr: Attr)(
    implicit D: Corecursive.Aux[F[DataF], DataF]): Either[InvalidDefaultAttributeValue, F[DataF]] =
    attr.unFix match {
      case NumericPropertyF(v) =>
        Either
          .catchNonFatal(D.embed(IntegerDataF(v.toLong)))
          .leftMap(err => InvalidDefaultAttributeValue(s"Failed to parse default for integer: ${err.getMessage}"))

      case _ => Left(InvalidDefaultAttributeValue("failed to parse default for integer"))
    }
}

class DefaultRules extends DefaultBoolean with DefaultInteger

object DefaultRules {
  case class InvalidDefaultAttributeValue(msg: String)
}

class DefaultsAttempt3(rules: DefaultRules) {
  import DefaultRules._
  import turtles.implicits._

  val blah: Fix[DataF] = Fix[DataF](NullDataF)

  //At every node in the schema adt, annotate with the default value
  val annotate: Algebra[SchemaF, Cofree[SchemaF, Either[InvalidDefaultAttributeValue, Fix[DataF]]]] = {
    case s @ ValueF(BooleanType, attributes) if attributes.contains(common.default) =>
      val zzz = rules.boolean[Fix](attributes(common.default))
      Cofree.apply[SchemaF, Either[InvalidDefaultAttributeValue, Fix[DataF]]](zzz, Eval.now(s))
    case z => Cofree(Right(blah), Eval.now(z))
  }

  val annotateM: AlgebraM[Either[InvalidDefaultAttributeValue, ?], SchemaF, Cofree[SchemaF, Fix[DataF]]] = {
    case s @ ValueF(BooleanType, attributes) if attributes.contains(common.default) =>
      rules.boolean[Fix](attributes(common.default)).map { default =>
        Cofree[SchemaF, Fix[DataF]](default, Eval.now(s))
      }

    case s @ ValueF(IntegerType, attributes) if attributes.contains(common.default) =>
      rules.integer[Fix](attributes(common.default)).map { default =>
        Cofree[SchemaF, Fix[DataF]](default, Eval.now(s))
      }

    case x => Right(Cofree[SchemaF, Fix[DataF]](blah, Eval.now(x)))
  }
}
