package datum.blue.schema.json

import datum.blue.data._
import datum.blue.ops.DefaultRules.InvalidDefaultAttributeValue
import datum.blue.ops.{DefaultRules, DefaultsAttempt3}
import datum.blue.schema._
import io.circe.Decoder.Result
import io.circe.{DecodingFailure, Json, JsonObject}
import turtles.Algebra
import turtles.data.Fix
import turtles.implicits._
import turtles.patterns.EnvT

import scala.collection.immutable.SortedMap

class Parse4(rules: DefaultRules) {
  import cats.syntax.either._
  import cats.instances.either._

  private def withDefault[A](check: Result[Fix[DataF]], default: Fix[DataF]): Result[Fix[DataF]] = {
    check match {
      case Right(x) if x.unFix == NullDataF      => Right(default)
      case ok @ Right(_)                         => ok
      case Left(_) if default.unFix != NullDataF => Right(default)
      case otherwise                             => otherwise
    }
  }

  private val algebra2: Algebra[EnvT[Fix[DataF], SchemaF, ?], Json => Result[Fix[DataF]]] = {
    case EnvT((default, thingy)) =>
      thingy match {
        case ValueF(BooleanType, attrs) =>
          js =>
            val bleh = js.as[Boolean].map(v => Fix[DataF](BooleanDataF(v)))
            withDefault(bleh, default)

        case ValueF(TextType, attrs) => _.as[String].map(v => Fix[DataF](TextDataF(v)))

        case ValueF(IntegerType, attrs) =>
          js =>
            val bleh = js.as[String].map(v => Fix[DataF](TextDataF(v)))
            withDefault(bleh, default)

        case StructF(fields, attrs) =>
          _.as[JsonObject].flatMap { obj =>
            var error: Option[DecodingFailure] = None
            val collected = fields.iterator
              .map { case (key, fn) => (key, fn(obj(key).getOrElse(Json.Null))) }
              .foldLeft(SortedMap.newBuilder[String, Fix[DataF]]) {
                case (acc, (k, Right(d))) => acc += (k -> d)
                case (acc, (k, Left(err))) =>
                  error = Some(DecodingFailure(s"Error for key: $k (error was: ${err.message})", err.history))
                  acc
              }
            if (error.isDefined) Left(error.get)
            else Right(Fix[DataF](StructDataF(collected.result())))
          }
      }
  }

  def apply(sch: Fix[SchemaF]): Either[InvalidDefaultAttributeValue, Json => Result[Fix[DataF]]] = {
    val foo = new DefaultsAttempt3(rules)
    val bar = sch.cataM(foo.annotateM)
    bar.map(_.cata(algebra2))
  }

}
