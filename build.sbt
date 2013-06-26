scalaVersion in ThisBuild := "2.10.2"

scalacOptions in Compile in ThisBuild ++= Seq("-uniqid")

lazy val root = project in file(".") aggregate(macros, main)

lazy val macros = project settings (
  libraryDependencies ++=
    Seq("reflect", "compiler").map(a => "org.scala-lang" % s"scala-$a" % scalaVersion.value)
)

lazy val main = project dependsOn(macros) settings(
   scalacOptions in Compile ++= Seq("-Xprint:all", "-Yshow-trees")
)
