lazy val commonSettings = Seq(
  name := "datum",
  scalaVersion := "2.12.6",
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7"),
  scalacOptions ++= Seq(
    "-Xlint",
    "-deprecation",
    "-unchecked",
    "-optimise",
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
      "org.typelevel" %% "alleycats-core" % "1.1.0",
      "io.github.davidgregory084" %% "schemes-core" % "0.2.0",
      "org.technomadic" %% "turtles-core" % "0.1.0",
      "com.lihaoyi" %% "pprint" % "0.5.2",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )

lazy val json = (project in file("json"))
  .settings(commonSettings)
  .settings(
    name := "datum-json",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" %  "0.9.3",
      "io.circe" %% "circe-parser" %  "0.9.3"
    )
  )
  .dependsOn(core)

