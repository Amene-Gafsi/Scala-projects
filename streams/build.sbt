import org.scalajs.linker.interface.ModuleInitializer

lazy val sharedSetting = Seq(
  scalaVersion := "3.3.1",
  scalacOptions ++= Seq("-language:implicitConversions", "-deprecation", "-Xfatal-warnings"),
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0-M10" % Test
)

lazy val root = project.in(file("."))
  .settings(
    sharedSetting
  )
  .aggregate(streams.jvm)

lazy val streams = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(
    sharedSetting,
    test := {}
  )
  .jsSettings(
    test := {}
  )


lazy val web = project.in(file("web"))
  .enablePlugins(ScalaJSPlugin)
  .settings(sharedSetting)
  .settings(
    // This is an application with a main method
    scalaJSUseMainModuleInitializer := true,
    // ECMAScript
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0",
    test := {}
  )
  .dependsOn(streams.js)
