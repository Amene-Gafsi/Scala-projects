name := "find-lazy"
scalaVersion := "3.3.1"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.1"
libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M8" % Test
libraryDependencies += "org.scalameta" % "scalameta_2.13" % "4.8.11"
scalacOptions ++= Seq("-Xfatal-warnings")
