package datum.blue.schema.json

import datum.blue.attributes.Attributes
import datum.blue.data._
import datum.blue.ops.SuperAbstract
import datum.blue.schema.SchemaF
import io.circe.Decoder.Result
import io.circe.{DecodingFailure, Json, JsonObject}
import turtles.{Corecursive, Recursive}

import scala.collection.immutable.SortedMap

object Parse2 {

  private class JsonSchemaFolder[Data]()(implicit Data: Corecursive.Aux[Data, DataF])
    extends SuperAbstract.Folder[Json => Result[Data]] {

    override def onBoolean: Json => Result[Data] =
      _.as[Boolean].map(v => Data.embed(BooleanDataF(v)))

    override def onText: Json => Result[Data] =
      _.as[String].map(v => Data.embed(TextDataF(v)))

    override def onReal: Json => Result[Data] =
      _.as[Double].map(v => Data.embed(RealDataF(v)))

    override def onInteger: Json => Result[Data] =
      _.as[Long].map(v => Data.embed(IntegerDataF(v)))

    override def onStruct(fields: SortedMap[String, Json => Result[Data]]): Json => Result[Data] =
      _.as[JsonObject].flatMap { obj =>
        var error: Option[DecodingFailure] = None
        val collected = fields.iterator
          .map {
            case (k, func) =>
              val selected = obj(k).getOrElse(Json.Null)
              (k, func(selected))
          }
          .foldLeft(SortedMap.newBuilder[String, Data]) {
            case (acc, (k, Right(d))) =>
              acc += (k -> d)

            case (acc, (k, Left(err))) =>
              error = Some(DecodingFailure(s"Error for key: $k (error was: ${err.message})", err.history))
              acc
          }

        if (error.isDefined) Left(error.get)
        else Right(Data.embed(StructDataF(collected.result())))
      }
  }

  def apply[F[_[_]]](sch: F[SchemaF])(
    checks: PartialFunction[(Result[F[DataF]], Attributes), Result[F[DataF]]]
  )(js: Json)(implicit Schema: Recursive.Aux[F[SchemaF], SchemaF], Data: Corecursive.Aux[F[DataF], DataF]): Result[F[DataF]] = {
    val folder = new JsonSchemaFolder[F[DataF]]()
    val alg = SuperAbstract.function1[Json, Result[F[DataF]]](folder)(checks)
    val parseFunction = Schema.cata(sch)(alg)
    parseFunction(js)
  }
}
