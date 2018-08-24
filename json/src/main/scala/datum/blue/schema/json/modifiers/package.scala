package datum.blue.schema.json

import cats.~>
import datum.blue.attributes
import datum.blue.attributes._
import datum.blue.data._
import datum.blue.schema.{BooleanType, SchemaF, ValueF}
import io.circe.Decoder.Result
import io.circe.DecodingFailure
import turtles.data.Fix
import turtles.{Algebra, Birecursive, Corecursive, GAlgebra, Recursive}
import turtles.implicits._

import scala.collection.immutable.SortedMap

package object modifiers {

  def optional[F[_[_]]](
    in: Attributed[Result[F[DataF]]]
  )(implicit Data: Corecursive.Aux[F[DataF], DataF]): Attributed[Result[F[DataF]]] = {
    in.transform {
      case (attrs, result) =>
        val isOptional = attrs.contains(common.optional) && attrs(common.optional) == attributes.property(true)

        val update =
          if (isOptional && result.isLeft) Right(Data.embed(NullDataF))
          else result

        (attrs, update)
    }
  }

  def qqzz[F[_[_]]](implicit Data: Birecursive.Aux[F[DataF], DataF]): Algebra[AttrF, Result[F[DataF]]] = {
    case BooleanPropertyF(v) => Right(Data.embed(BooleanDataF(v)))
    case NumericPropertyF(v) => Right(Data.embed(RealDataF(v)))
    case TextPropertyF(v)    => Right(Data.embed(TextDataF(v)))
    case _                   => Left(DecodingFailure("unknown default argument", List.empty))
  }

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

  def withDefaults[F[_[_]]](
    in: Attributed[Result[F[DataF]]]
  )(implicit Data: Birecursive.Aux[F[DataF], DataF]): Attributed[Result[F[DataF]]] = {
    in.transform {
      case (attrs, result) if result.isLeft || result == Right(Data.embed(NullDataF)) =>
        val key = AttrKey("default")
        val update =
          if (attrs.contains(key)) attrs(key).cata(qqzz)
          else result
        (attrs, update)
      case otherwise => otherwise
    }
  }


}
