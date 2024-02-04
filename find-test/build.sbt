name := "find-test"
scalaVersion := "3.3.1"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.1"
libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M10" % Test
enablePlugins(PackPlugin)
scalacOptions ++= Seq("-Xfatal-warnings")
