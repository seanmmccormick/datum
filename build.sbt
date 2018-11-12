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
    "com.lihaoyi" %% "pprint" % "0.5.2",
    "org.typelevel" %% "cats-core" % "1.4.0",
    "org.typelevel" %% "alleycats-core" % "1.4.0",
    "io.higherkindness" %% "droste-core" % "0.5.0",
    "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    "org.typelevel" %% "cats-effect" % "1.0.0",
    "io.chrisdavenport" %% "cats-scalacheck" % "0.1.0" % Test,
    "com.47deg" %% "scalacheck-toolbox-datetime" % "0.2.5" % Test
  )
)

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