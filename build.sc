// build.sc
import mill._
import mill.define.Target
import mill.scalalib._

trait CommonSettings extends ScalaModule {

  def scalaVersion = "2.12.4"

  override def scalacOptions = Seq(
    "-Ypartial-unification"
  )

  override def scalacPluginIvyDeps = Agg(ivy"org.spire-math::kind-projector:0.9.7") //remember to change this for cross compile

  val commonDeps = Agg(
    ivy"io.github.davidgregory084::schemes-core:0.2.0",
    ivy"org.technomadic::turtles-core:0.1.0",
    ivy"com.lihaoyi::pprint:0.5.2"
  )
}

object core extends CommonSettings {
  override def ivyDeps = commonDeps
}

object streams extends CommonSettings {
  def ivyDeps = commonDeps ++ Seq(
    ivy"com.typesafe.akka::akka-stream:2.5.13",
  )

  def moduleDeps = Seq(core)
}

object json extends CommonSettings {
  def ivyDeps = commonDeps ++ Seq(
    ivy"com.lihaoyi::ujson:0.6.6"
  )

  def moduleDeps = Seq(core)
}

object lens extends CommonSettings {
  val monocleVersion = "1.5.0"

  def ivyDeps = commonDeps ++ Seq(
    ivy"com.github.julien-truffaut::monocle-core:$monocleVersion"
  )

  def moduleDeps = Seq(core)
}

object challenges extends CommonSettings {
  def moduleDeps = Seq(core, lens, json)
}