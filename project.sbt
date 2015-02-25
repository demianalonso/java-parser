name := "parsers"

description := ""

scalaVersion := "2.11.1"

name := "java-parser"

version := "1.0"

scalaVersion := "2.11.4"

///////////////////////////////////////////////////////////////////////////////////////////////////

lazy val cacao = FDProject(
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.parboiled" %% "parboiled" % "2.1.0"
)

///////////////////////////////////////////////////////////////////////////////////////////////////

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)

scalacOptions += "-feature"

licenses += "LGPLv3" -> url("https://www.gnu.org/licenses/lgpl.html")