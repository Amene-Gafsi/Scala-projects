scalaVersion := "3.3.1"
scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies ++= Seq(
  ("com.storm-enroute" %% "scalameter-core" % "0.21").cross(CrossVersion.for3Use2_13),
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3",
  "org.scalameta" %% "munit" % "0.7.26" % Test
)

// scalafix
inThisBuild(
  List(
    scalaVersion := "3.3.1",
    semanticdbEnabled := true
  )
)
