name := "anagrams"

scalaVersion := "3.3.1"

scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "os-lib" % "0.9.1",
  "org.scalameta" %% "munit" % "1.0.0-M10" % Test,
  "org.scala-lang.modules" %% "scala-java8-compat" % "1.0.0"
)
