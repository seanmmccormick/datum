package demo

import datum.blue.schema
import datum.blue.schema.{Column, IntegerType, RealType, TextType}
import datum.blue.schema.json.Writer._
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
import io.circe.Decoder.Result

sealed trait DemoCommand

case class Show(
  file: String,
) extends DemoCommand

case class Transform(
  src: String,
  schema: String,
  using: String
) extends DemoCommand

object Demo extends CommandApp[DemoCommand] {

  val fix = new schema.Specialize[Fix[schema.SchemaF]]

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

  def show(file: String): Unit = {
    val path = pwd / 'samples / file
    println(read(path))
  }

  def transform(src: String, sch: String, using: String): Unit =  {
    val srcPath = pwd / 'samples / src
    val schemaPath = pwd / 'samples / sch
    val transformPath = pwd / 'samples / using

    val parsed = read(srcPath).split("\n").toList.traverse[Either[ParsingFailure, ?], Json] {
      txt => io.circe.parser.parse(txt)
    }

    pprint.pprintln(parsed)

  }

  def run(command: DemoCommand, args: RemainingArgs): Unit = command match {
    case Show(file)                 => show(file)
    case Transform(src, sch, using) => transform(src, sch, using)
  }
}
