
val dottyVersion = "3.0.0-M2"
//val dottyVersion = dottyLatestNightlyBuild.get

lazy val root = project
  .in(file("."))
  .settings(
    name := "algorithmic-algebras-embedding",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    //scalacOptions ++= Seq( "-Ydebug:implicits", "-Ydebug-trace", "-Ydebug-names", "-Ylog:typer", "-Yplain-printer" ),
    scalacOptions ++= Seq( "-unchecked", "-explain-types",  "-Ydebug-trace", "-Ydebug-names"  ),

    libraryDependencies += "org.scala-lang" %% "scala3-staging" % dottyVersion,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.19" % Test,
    //libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
    testFrameworks += new TestFramework("munit.Framework")
  )




