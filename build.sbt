val buildOrganization = "org.tearne"
val buildVersion      = "0.2.18"
val buildScalaVersion = "2.11.8"

val akkaVersion       = "2.4.6"
val logbackClassic    = "ch.qos.logback" % "logback-classic" % "1.1.1"
val commonsIo         = "commons-io" % "commons-io" % "2.4"
val commonsMath3      = "org.apache.commons" % "commons-math3" % "3.2"
val playJson          = "com.typesafe.play" %% "play-json" % "2.4.6" exclude("org.slf4j", "slf4j-simple")

EclipseKeys.withSource := true
EclipseKeys.skipParents in ThisBuild := false

lazy val commonSettings = Seq(
  organization := buildOrganization,
  version      := buildVersion,
  licenses     += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html")),
  scalaVersion := buildScalaVersion,
  scalacOptions ++= Seq(
    "-unchecked" 
    ,"-deprecation" 
    ,"-feature"
 //   ,"-encoding" 
 //   ,"UTF-8" 
 //   ,"-Xlint"
   ),

  //Copy all dependencies to lib_managed
  retrieveManaged	:= true,

  libraryDependencies ++= Seq(
    "junit" % "junit" % "4.8" % "test->default",
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.mockito" % "mockito-all" % "1.9.0" %"test->default",
    "org.slf4j" % "slf4j-api" % "1.7.7",
    "com.novocode" % "junit-interface" % "0.11" % "test"
  ),
  
  //Doesn't seem to work
  libraryDependencies ~= { _.map(_.exclude("org.slf4j", "slf4j-nop")) }
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
  .aggregate(core, examples, abc, r, spike)
  .settings(commonSettings: _*)
  .settings(publish := { })

lazy val core = project.in(file("sampler-core"))
  .settings(
    name := "sampler-core"
  )
  .settings(
    libraryDependencies ++= Seq(
      commonsMath3,
      playJson,
      "org.scalaz" %% "scalaz-core" % "7.1.0",
      "org.spire-math" %% "spire" % "0.11.0"
    )
  )
  .settings(commonSettings: _*)
  .settings(packageSettings: _*)

lazy val r = (project in file("sampler-r"))
  .dependsOn(core)
  .settings(name := "sampler-r")
  .settings(
    libraryDependencies ++= Seq(
      "org.rosuda.REngine" % "Rserve" % "1.8.1",
      logbackClassic
    )
  )
  .settings(commonSettings: _*)
  .settings(packageSettings: _*)
  
lazy val examples = project.in(file("sampler-examples"))
  .dependsOn(core, abc, r)
  .settings(name := "sampler-examples")
  .settings(
    libraryDependencies ++= Seq(
      commonsMath3,
      logbackClassic,
      commonsIo,
      //TODO switch to play json
      "org.json4s" %% "json4s-native" % "3.2.10",
      "org.apache.jclouds.provider" % "aws-ec2" % "1.9.1",
      "org.apache.jclouds.provider" % "softlayer" % "1.9.1"
    ),
    mainClass in Compile := Some("sampler.example.abc.UnfairCoin")
  )
  .settings(commonSettings: _*)
  .settings(packageSettings: _*)
  .enablePlugins(JavaAppPackaging)

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
      "org.apache.jclouds" % "jclouds-core" % "1.9.2",
      "org.apache.jclouds" % "jclouds-compute" % "1.9.2",
      "org.apache.jclouds.driver" % "jclouds-slf4j" % "1.9.1",
      "com.github.scopt" %% "scopt" % "3.3.0"
    )
  )
  .settings(commonSettings: _*)

  
lazy val spike = project.in(file("sampler-spike"))
  .dependsOn(core, r)
  .settings(name := "sampler-spike")
  .settings(publish := { })
  .settings(
    libraryDependencies ++= Seq(
      commonsMath3,
      logbackClassic,
      "org.json4s" %% "json4s-native" % "3.2.11",
      "org.freemarker" % "freemarker" % "2.3.21",
      "org.typelevel" %% "cats" % "0.4.1"
    )
  )
  .settings(commonSettings: _*)