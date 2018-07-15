package datum.blue.transform.json

import datum.blue.transform
import datum.blue.transform.TransformF
import io.circe.Decoder.Result
import io.circe._
import turtles.{CoalgebraM, Corecursive}
import cats.instances.either._

import scala.collection.immutable.SortedMap

object Reader {

  def decodeWith(obj: JsonObject, name: String)(f: Json => Result[TransformF[HCursor]]): Result[TransformF[HCursor]] = {
    obj(name)
      .toRight[DecodingFailure](DecodingFailure(s"failed to decode $name transform", List.empty))
      .flatMap(f)
  }

  def coalgebra[R](implicit R: Corecursive.Aux[R, TransformF]): CoalgebraM[Result, TransformF, HCursor] = {
    case cur if cur.value.isString =>
      cur.as[String].map {
        case "keep" => transform.KeepF

        case "drop" => transform.DropF
      }

    case cur if cur.value.isObject =>
      cur.as[JsonObject].flatMap { obj =>
        if (obj.contains("struct")) {
          decodeWith(obj, "struct") {
            _.as[SortedMap[String, HCursor]].map(transform.StructF.apply)
          }
        } else if (obj.contains("explode")) {
          decodeWith(obj, "explode") {
            _.as[HCursor].map(transform.ExplodeF.apply)
          }
        } else if (obj.contains("select")) {
          decodeWith(obj, "select") { js =>
            for {
              field <- js.hcursor.downField("field").as[String]
              target <- js.hcursor.downField("target").as[HCursor]
            } yield transform.SelectFieldF(field, target)
          }
        }
        else {
          pprint.pprintln("Todo: This better")
          pprint.pprintln(obj)
          ???
        }
      }
  }

  implicit def decoder[R](implicit R: Corecursive.Aux[R, TransformF]): Decoder[R] = new Decoder[R] {
    override def apply(c: HCursor): Result[R] = R.anaM(c)(coalgebra)
  }
}
