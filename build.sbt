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
    "-Ypatmat-exhaust-depth",
    "40"
  ),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsV,
    "org.typelevel" %% "alleycats-core" % catsV,
    "io.higherkindness" %% "droste-core" % drosteV,
    "com.lihaoyi" %% "pprint" % "0.5.2",
    "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    "com.47deg" %% "scalacheck-toolbox-datetime" % "0.2.5" % Test
  )
)

// Modules
lazy val datum = (project in file("."))
  .aggregate(core, gen, testCore, ujson)

lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(
    name := "datum-core",
  )

lazy val gen = (project in file("gen"))
  .settings(commonSettings)
  .settings(
    name := "datum-gen",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.5",
      "io.chrisdavenport" %% "cats-scalacheck" % "0.1.0",
      "com.47deg" %% "scalacheck-toolbox-datetime" % "0.2.5"
    )
  )
  .dependsOn(core)

// sub project so that test code can depend on both core and gen
// otherwise there is a circular dependency
lazy val testCore = (project in file("test-core"))
  .settings(commonSettings)
  .settings(
    name := "datum-core-test"
  )
  .dependsOn(core, gen % Test)

lazy val ujson = (project in file("ujson"))
  .settings(commonSettings)
  .settings(
    name := "datum-ujson",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "0.6.6"
    )
  )
  .dependsOn(core, gen % Test)
