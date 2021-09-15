
val dottyVersion = "3.0.1"
//val dottyVersion = dottyLatestNightlyBuild.get

lazy val root = project
  .in(file("."))
  .settings(
    name := "algorithmic-algebras-embedding",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    //scalacOptions ++= Seq( "-unchecked", "-explain-types",  "-Ydebug-trace", "-Ydebug-names"  ),

    //scalacOptions ++= Seq( "-unchecked"),
    Compile / doc / scalacOptions := Seq("-groups",
                "-source-links:github://ips-kyiv/algorithmic-algebras-embedding/master",
                "-siteroot","docs"
                ),
    libraryDependencies += "org.scala-lang" %% "scala3-staging" % dottyVersion,
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.26" % Test,
    testFrameworks += new TestFramework("munit.Framework"),
    resolvers += Resolver.sbtPluginRepo("releases"),
    git.remoteRepo := "git@github.com:ips-kyiv/algorithmic-algebras-embedding.git"
  )
  .enablePlugins(SiteScaladocPlugin,
                  GhpagesPlugin)




