package demo

import cats.Functor
import datum.blue.ops.{TablePrint, TransformData, TransformSchema}
import datum.blue.schema
import datum.blue.transform

// todo: clean up circe imports for library types
import datum.blue.transform.json.Writer._
import datum.blue.transform.json.Reader._
import datum.blue.schema.json.Writer._
import datum.blue.schema.json.Reader._

import datum.blue.schema._
import datum.blue.transform.TransformF
import ammonite.ops._
import caseapp._
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import turtles.data.Fix
import turtles._
import cats.syntax.traverse._
import cats.instances.either._
import cats.instances.list._
import datum.blue.data.DataF
import io.circe.Decoder.Result

sealed trait DemoCommand

case class Show(
  src: String,
  schema: String,
  using: String
) extends DemoCommand

case class Todo(
  src: String,
  schema: String,
  using: String
) extends DemoCommand

object Demo extends CommandApp[DemoCommand] {

  val fix = new schema.Specialize[Fix[schema.SchemaF]]

  val tra = new transform.Specialize[Fix[transform.TransformF]]

  implicit val fixthis = transform.json.Writer.encode[Fix[transform.TransformF]]
  implicit val fixthis2 = schema.json.Writer.encode[Fix[schema.SchemaF]]
  implicit val wtf = transform.json.Reader.decoder[Fix[transform.TransformF]]
  implicit val wtf2 = schema.json.Reader.decoder[Fix[schema.SchemaF]]

  val aaa = fix.struct(
    "name" -> fix.value(TextType),
    "age" -> fix.value(IntegerType),
    "test" -> fix.row(
      Column(fix.value(TextType), Some("Foo")),
      Column(fix.value(RealType), Some("Bar"))
    )(Map.empty),
    "school" -> fix.struct(
      "school_id" -> fix.value(IntegerType),
      "school_name" -> fix.value(TextType)
    )(Map.empty)
  )(Map.empty)

  val transform1 = tra.explode(
    tra.struct(
      "name" -> tra.keep,
      "age" -> tra.keep,
      "school" -> tra.select("school", tra.select("school_name", tra.keep))
    ))

  def show(schemaPath: String, transformPath: String): Unit = {
    println("\n== Transform Definition ==")
    println(read(pwd / 'samples / transformPath))
    println("\n== Schema Definition ==")
    println(read(pwd / 'samples / schemaPath))
  }

  def todo(dataSrc: String, schemaSrc: String, transformSrc: String): Unit = {
    val srcPath = pwd / 'samples / dataSrc
    val schemaPath = pwd / 'samples / schemaSrc
    val transformPath = pwd / 'samples / transformSrc

    val loadedSchema = parse(read(schemaPath)).flatMap(_.as[Fix[SchemaF]])

    val loadedTransform: Either[Error, Fix[TransformF]] = parse(read(transformPath)).flatMap(_.as[Fix[TransformF]])

    val newSchema: Either[Error, Fix[SchemaF]] = for {
      sch <- loadedSchema
      trans <- loadedTransform
      newSch <- TransformSchema(trans)(sch).toRight[Error](
        DecodingFailure("Failed to transform the schema!", List.empty)) //todo figure out a better kind of error
    } yield newSch

    val parsed = read(srcPath).split("\n").toList.traverse[Either[Error, ?], Fix[DataF]] { txt =>
      for {
        js <- io.circe.parser.parse(txt)
        sch <- loadedSchema
        decoder = schema.json.Parse2(sch)(schema.json.checks.optional) _
        data <- decoder(js)
      } yield {
        data
      }
    }

    //todo - move this logic into a pure function not in the Either monad
    val result = for {
      data <- parsed
      trans <- loadedTransform
      newSch <- newSchema
      r <- data.traverse[Either[Error, ?], Fix[DataF]] { d =>
        TransformData(trans)(d).toRight[Error](DecodingFailure("Failed to transform row", List.empty))
      }
    } yield (r, newSch)

    val output = result match {
      case Right((ok, ns)) =>
        println("== Transformed Schema ==")
        println(ns.asJson.spaces4)
        println("\n== Transformed Data ==")
        TablePrint(ns, ok)
      case Left(err) => s"Transformation failed: $err"
    }

    println(output)
  }

  def run(command: DemoCommand, args: RemainingArgs): Unit = command match {
    case Show(_, sch, using)   => show(sch, using)
    case Todo(src, sch, using) => todo(src, sch, using)
  }
}
