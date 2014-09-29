scalaJSSettings

name := "enchantexample"

version := "0.1-SNAPSHOT"

organization := "com.geishatokyo.scalajs"

crossScalaVersions := Seq("2.11.0")

licenses += ("MIT", url("http://opensource.org/licenses/mit-license.php"))

libraryDependencies ++= Seq(
  "org.scala-lang.modules.scalajs" %% "scalajs-dom" % "0.3"
)
