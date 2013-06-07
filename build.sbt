scalaVersion in ThisBuild := "2.10.1"

lazy val root = project in file(".") aggregate(macros, main)

lazy val macros = project settings (
	libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
	libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
)

lazy val main = project dependsOn(macros)