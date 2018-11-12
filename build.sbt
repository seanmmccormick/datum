
// Versions
val catsV = "1.4.0"
val drosteV = "0.5.0"

// Settings
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
    "-Ypartial-unification",
    "-Ypatmat-exhaust-depth","40"
  ),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsV,
    "org.typelevel" %% "alleycats-core" % catsV,
    "io.higherkindness" %% "droste-core" % drosteV,
    "com.lihaoyi" %% "pprint" % "0.5.2",
    "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    "io.chrisdavenport" %% "cats-scalacheck" % "0.1.0" % Test,
    "com.47deg" %% "scalacheck-toolbox-datetime" % "0.2.5" % Test
  )
)

// Modules
lazy val root = (project in file("."))
  .aggregate(core, ujson)

lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(
    name := "datum-core",
  )

lazy val ujson = (project in file("ujson"))
  .settings(commonSettings)
  .settings(
    name := "datum-ujson",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "0.6.6"
    )
  )
  .dependsOn(core)