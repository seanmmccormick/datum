lazy val commonSettings = Seq(
  name := "datum",
  scalaVersion := "2.12.6",
  organization := "com.voltir",
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7"),
  scalacOptions ++= Seq(
    "-Xlint",
    "-deprecation",
    "-unchecked",
    "-Ypartial-unification"
  ),
  libraryDependencies ++= Seq(
    "org.technomadic" %% "turtles-core" % "0.1.0",
    "com.lihaoyi" %% "pprint" % "0.5.2",
    "org.scalatest" %% "scalatest" % "3.0.5" % Test
  )
)

lazy val root = (project in file("."))
  .aggregate(core, json)

lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(
    name := "datum-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.4.0",
      "org.typelevel" %% "alleycats-core" % "1.4.0",
      "io.higherkindness" %% "droste-core" % "0.5.0",
      "org.typelevel" %% "cats-effect" % "1.0.0",
      "io.github.davidgregory084" %% "schemes-core" % "0.2.0",
      "org.technomadic" %% "turtles-core" % "0.1.0",
      "com.lihaoyi" %% "pprint" % "0.5.2",
      "io.chrisdavenport" %% "cats-scalacheck" % "0.1.0" % Test,
      "com.47deg" %% "scalacheck-toolbox-datetime" % "0.2.5" % Test,
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )

lazy val json = (project in file("json"))
  .settings(commonSettings)
  .settings(
    name := "datum-json",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.9.3",
      "io.circe" %% "circe-parser" % "0.9.3"
    )
  )
  .dependsOn(core)

lazy val ujson = (project in file("ujson"))
  .settings(commonSettings)
  .settings(
    name := "datum-ujson",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "0.6.6"
    )
  )
  .dependsOn(core)

lazy val demo = (project in file("demo"))
  .settings(commonSettings)
  .settings(
    fork := true,
    outputStrategy := Some(StdoutOutput),
    name := "datum-demo",
    libraryDependencies ++= Seq(
      "com.github.alexarchambault" %% "case-app" % "2.0.0-M3",
      "com.lihaoyi" %% "ammonite-ops" % "1.1.2"
    )
  )
  .dependsOn(core, json)
