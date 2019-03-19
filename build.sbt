val buildOrganization  = "org.tearne"
val buildVersion       = "0.3.15"
val buildScalaVersion  = "2.12.6"
val crossBuildVersions = Seq(buildScalaVersion, "2.11.12")

val typesafeConfig    = "com.typesafe" % "config" % "1.3.2"
val akkaVersion       = "2.5.7"
val logbackClassic    = "ch.qos.logback" % "logback-classic" % "1.1.1"
val commonsIo         = "commons-io" % "commons-io" % "2.4"
val commonsMath3      = "org.apache.commons" % "commons-math3" % "3.2"
val playJson          = "com.typesafe.play" %% "play-json" % "2.6.7" //exclude("org.slf4j", "slf4j-simple")
val cats              = "org.typelevel" %% "cats-core" % "1.0.0-RC1" withSources()
val rServe            = "org.rosuda.REngine" % "Rserve" % "1.8.1"

val scalaTest         = "org.scalatest" %% "scalatest" % "3.0.4" % "test"
val scalaCheck        = "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

lazy val commonSettings = Seq(
  organization := buildOrganization,
  version      := buildVersion,
  licenses     += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html")),
  scalaVersion := buildScalaVersion,
  crossScalaVersions := crossBuildVersions,
  scalacOptions ++= Seq(
    "-unchecked" 
    ,"-deprecation" 
    ,"-feature"
//    ,"-encoding"
//    ,"UTF-8"
//    ,"-Xlint"
   ),

//  retrieveManaged	:= true,  //Copy dependencies to lib_managed

  libraryDependencies ++= Seq(
    "junit" % "junit" % "4.8" % "test->default",
    scalaTest,
    scalaCheck,
    typesafeConfig,
    "org.mockito" % "mockito-core" % "2.12.0" % Test,
    "org.slf4j" % "slf4j-api" % "1.7.25"
  )
)

lazy val packageSettings = Seq(
  mappings in (Compile,packageBin) ~= { (ms: Seq[(File, String)]) =>
    ms filter { case (file, toPath) =>
      if(toPath.contains(".xml") || toPath.contains(".config") || toPath.contains(".json")){
        println("> excluding: "+toPath)
        false
      }
      else true
    }
  }
)

lazy val root = project.in(file("."))
  .aggregate(core, examples, abc)//, spike)
  .settings(commonSettings: _*)

lazy val core = project.in(file("sampler-core"))
  .settings(
    name := "sampler-core"
  )
  .settings(
    libraryDependencies ++= Seq(
      commonsMath3,
      playJson,
      rServe,
      "org.typelevel" %% "spire" % "0.14.1",
      cats
    )
  )
  .settings(commonSettings: _*)
  .settings(packageSettings: _*)

lazy val examples = project.in(file("sampler-examples"))
  .dependsOn(core, abc)
  .settings(name := "sampler-examples")
  .settings(
    libraryDependencies ++= Seq(
      commonsMath3,
      logbackClassic,
      commonsIo
    ),
    mainClass in Compile := Some("sampler.example.abc.UnfairCoin")
  )
  .settings(commonSettings: _*)
  .settings(packageSettings: _*)

lazy val abc = project.in(file("sampler-abc"))
  .dependsOn(core)
  .settings(name := "sampler-abc")
  .settings(
    libraryDependencies ++= Seq(
      commonsMath3,
      commonsIo,
      playJson,
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-remote" % akkaVersion,
      "com.typesafe.akka" %% "akka-cluster" % akkaVersion,
      "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test",
      "com.fasterxml.uuid" % "java-uuid-generator" % "3.1.4"
    )
  )
  .settings(commonSettings: _*)


lazy val spike = project.in(file("sampler-spike"))
  .dependsOn(core)
  .settings(name := "sampler-spike")
  .settings(publish := { })
  .settings(
    libraryDependencies ++= Seq(
      commonsMath3,
      logbackClassic,
      "org.freemarker" % "freemarker" % "2.3.21",
      cats
    )
  )
  .settings(commonSettings: _*)
