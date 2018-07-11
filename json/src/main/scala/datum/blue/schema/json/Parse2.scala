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
        var error: Option[String] = None
        pprint.pprintln(fields)
        val collected = fields.iterator
          .map {
            case (k, func) =>
              val selected = obj(k).getOrElse(Json.Null)
              (k, func(selected))
          }
          .foldLeft(SortedMap.empty[String, Data]) {
            case (acc, (k, Right(d))) =>
              acc + (k -> d)

            case (_, (err, _)) =>
              error = Some(err)
              SortedMap.empty
          }

        if (error.isDefined) Left(DecodingFailure(s"Missing expected key: ${error.get}", List.empty))
        else Right(Data.embed(StructDataF(collected)))
      }
  }

  def apply[F[_[_]]](sch: F[SchemaF], js: Json)(
    checks: PartialFunction[(Result[F[DataF]], Attributes), Result[F[DataF]]]
  )(implicit Schema: Recursive.Aux[F[SchemaF], SchemaF], Data: Corecursive.Aux[F[DataF], DataF]): Result[F[DataF]] = {
    val folder = new JsonSchemaFolder[F[DataF]]()
    val alg = SuperAbstract.function1[Json, Result[F[DataF]]](folder)(checks)
    val parseFunction = Schema.cata(sch)(alg)
    parseFunction(js)
  }
}
