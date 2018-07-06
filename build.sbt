lazy val commonSettings = Seq(
  name := "datum",
  scalaVersion := "2.12.4",
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7"),
  scalacOptions ++= Seq(
    "-Xlint",
    "-deprecation",
    "-unchecked",
    "-optimise",
    "-Ypartial-unification"
  )
)

lazy val root = (project in file("."))
  .aggregate(core)

lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(
    name := "datum-core",
    libraryDependencies ++= Seq(
      "io.github.davidgregory084" %% "schemes-core" % "0.2.0",
      "org.technomadic" %% "turtles-core" % "0.1.0",
      "com.lihaoyi" %% "pprint" % "0.5.2",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )
