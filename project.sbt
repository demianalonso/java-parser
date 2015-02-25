name := "java-parser"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test" withSources() withJavadoc()

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0" withSources() withJavadoc()
