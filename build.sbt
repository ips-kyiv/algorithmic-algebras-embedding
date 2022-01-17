
val dottyVersion = "3.1.0"
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
    // libraryDependencies += ("org.scalameta" %% "scalameta" % "4.4.28").cross(CrossVersion.for3Use2_13),
    libraryDependencies += "com.github.rssh" %% "dotty-cps-async" % "0.9.3",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.27" % Test,
    testFrameworks += new TestFramework("munit.Framework"),
    resolvers += Resolver.sbtPluginRepo("releases"),
    git.remoteRepo := "git@github.com:ips-kyiv/algorithmic-algebras-embedding.git"
  )
  .enablePlugins(SiteScaladocPlugin,
                  GhpagesPlugin)




