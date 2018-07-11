package datum.blue.schema.json

import alleycats.Empty
import datum.blue.attributes.Attributes
import datum.blue.data._
import datum.blue.schema._
import io.circe.Decoder.Result
import io.circe.{DecodingFailure, Json, JsonObject}
import turtles.{Algebra, Corecursive, Recursive}
import cats.syntax.traverse._
import cats.syntax.either._
import cats.instances.either._
import cats.instances.list._
import datum.blue.attributes

import scala.collection.immutable.SortedMap

object Parse {

  def algebra[Schema, Data](
    crazy: PartialFunction[(Result[Data], Attributes), Result[Data]]
  )(
    implicit Data: Corecursive.Aux[Data, DataF]
  ): Algebra[SchemaF, Json => Result[Data]] = {

    def checking(inp: Result[Data], attrs: Attributes): Result[Data] = {
      crazy.applyOrElse[(Result[Data], Attributes), Result[Data]]((inp, attrs), _ => inp)
    }

    {
      case ValueF(BooleanType, attrs) =>
        js =>
          checking(js.as[Boolean].map(x => Data.embed(BooleanDataF(x))), attrs)

      case ValueF(TextType, attrs) =>
        js =>
          checking(js.as[String].map(x => Data.embed(TextDataF(x))), attrs)

      case StructF(fields, attrs) =>
        js =>
          js.as[JsonObject]
            .flatMap { obj =>
              val result: Result[List[(String, Data)]] = fields.toList.traverse[Result, (String, Data)] {
                case (k, fieldResult) =>
                  fieldResult
                  fieldResult(obj(k).getOrElse(Json.Null))
                    .leftMap(_ => DecodingFailure(s"Missing expected key: $k", List.empty))
                    .map(ok => (k, ok))
              }
              result
            }
            .map { goal =>
              Data.embed(StructDataF(SortedMap(goal: _*)))
            }
    }
  }

  def apply[F[_[_]]](sch: F[SchemaF], js: Json)(
    checks: PartialFunction[(Result[F[DataF]], Attributes), Result[F[DataF]]]
  )(implicit Schema: Recursive.Aux[F[SchemaF], SchemaF], Data: Corecursive.Aux[F[DataF], DataF]): Result[F[DataF]] = {
    val parseFunction = Schema.cata(sch)(algebra(checks))
    parseFunction(js)
  }

  object checks {

    def optional[Data: Empty]: PartialFunction[(Result[Data], Attributes), Result[Data]] = {
      case (_, attr) if attributes.isOptional(attr) => Right(Empty[Data].empty)
    }
  }
}
