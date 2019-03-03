package datum.ujsonlib.data
import cats.Traverse
import datum.patterns.data._
import datum.patterns.{schemas, data => d}
import datum.patterns.schemas._
import qq.droste.{Algebra, Coalgebra, scheme}
import cats.syntax.either._
import cats.instances.either._
import cats.data.Chain
import datum.modifiers.Optional
import ujson.Js

import scala.collection.immutable.SortedMap

object ReadJs {

  private def fromJs(tpe: schemas.Type, js: Js.Value): Either[String, Data] = {
    (tpe, js) match {
      case (TextType, Js.Str(v))     => Right(d.text(v))
      case (BooleanType, Js.Bool(v)) => Right(d.boolean(v))
      case (IntType, Js.Num(v))      => Right(d.integer(v.toInt))
      case err                       => Left(s"Expected: ${schemas.Type.asString(tpe)} but got ${pprint.apply(err, 2)}")
    }
  }

  val algebra: Algebra[SchemaF, Js.Value => Either[String, Data]] = Algebra {
    case ValueF(tpe, _) =>
      js =>
        fromJs(tpe, js)
    case ObjF(schema, _) => {
      case Js.Obj(fields) =>
        val values = schema.foldLeft(Chain.empty[Either[String, (String, Data)]]) {
          case (acc, (k, fn)) =>
            acc.prepend(fn(fields.getOrElse(k, Js.Null)).map((k, _)))
        }
        Traverse[Chain].sequence(values).map { collected =>
          val builder = SortedMap.newBuilder[String, Data]
          collected.iterator.foreach { case (k, d) => builder += k -> d }
          d.obj(builder.result())
        }

      case _ => Left("todo")
    }
  }

  def optional(
    alg: Algebra[SchemaF, Js.Value => Either[String, Data]]
  ): Algebra[SchemaF, Js.Value => Either[String, Data]] = Algebra { schema => js =>
    val fn = alg(schema)
    fn(js) match {
      case Left(err) if schema.attributes.contains(Optional.key) => Right(d.empty)
      case otherwise                                             => otherwise
    }
  }

  def define(
    alg: Algebra[SchemaF, Js.Value => Either[String, Data]] = algebra
  ): Schema => Js.Value => Either[String, Data] = {
    scheme.cata(alg)
  }
}
