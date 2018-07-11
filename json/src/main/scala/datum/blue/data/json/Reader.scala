package datum.blue.data.json

import datum.blue.data._
import io.circe.Decoder.Result
import io.circe._
import cats.instances.either._
import turtles.{CoalgebraM, Corecursive}

import scala.collection.immutable.SortedMap

object Reader {

  // You probably don't want to use this
  def coalgebra[R](implicit R: Corecursive.Aux[R, DataF]): CoalgebraM[Result, DataF, HCursor] = cur => {
    cur.value.fold[Result[DataF[HCursor]]](
      jsonNull = Right(NullDataF),
      jsonBoolean = b => Right(BooleanDataF(b)),
      jsonNumber = n => Right(RealDataF(n.toDouble)),
      jsonString = t => Right(TextDataF(t)),
      jsonArray = arr => Right(RowDataF(arr.map(_.hcursor))),
      jsonObject = obj =>
        Right(
          StructDataF(
            SortedMap.apply[String, HCursor](obj.toIterable.view.map { case (k, v) => (k, v.hcursor) }.toSeq: _*)
          ))
    )
  }

  implicit def decoder[R](implicit R: Corecursive.Aux[R, DataF]): Decoder[R] = new Decoder[R] {
    override def apply(c: HCursor): Result[R] = R.anaM(c)(coalgebra)
  }
}
