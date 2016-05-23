val buildOrganization = "org.tearne"
val buildVersion = "0.2.14"
val buildScalaVersion	= "2.11.8"

val logbackClassic = "ch.qos.logback" % "logback-classic" % "1.1.1"
val commonsIO = "commons-io" % "commons-io" % "2.4"

lazy val commonSettings = Seq(
  organization := buildOrganization,
  version		 := buildVersion,
  licenses 	 += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html")),
  scalaVersion := buildScalaVersion,
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
//	scalacOptions ++= Seq("-encoding", "UTF-8", "-Xlint", "-deprecation", "-unchecked", "-feature")

  //Copy all dependencies to lib_managed
  retrieveManaged	:= true,

  resolvers ++= Seq(
    "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
    "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
  ),

  libraryDependencies ++= Seq(
    "com.typesafe" % "config" % "1.2.1",
    "junit" % "junit" % "4.8" % "test->default",
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.mockito" % "mockito-all" % "1.9.0" %"test->default",
    "org.slf4j" % "slf4j-api" % "1.7.7",
    "org.scalaz" %% "scalaz-core" % "7.1.0",
    "com.novocode" % "junit-interface" % "0.11" % "test",
    "org.spire-math" %% "spire" % "0.11.0",
    "com.typesafe.play" %% "play-json" % "2.4.6"    
  )
)

EclipseKeys.withSource := true
EclipseKeys.skipParents in ThisBuild := false

lazy val packageSettings = Seq(
  mappings in (Compile,packageBin) ~= { (ms: Seq[(File, String)]) =>
    ms filter { case (file, toPath) =>
      if(toPath.contains(".xml") || toPath.contains(".config")){
        println("> excluding: "+toPath)
        false
      }
      else true
    }
  }
)

lazy val root = project.in(file("."))
  .aggregate(core, examples, abc, arrr, spike)
  .settings(commonSettings: _*)

lazy val core = project.in(file("sampler-core"))
  .settings(commonSettings: _*)
  .settings(packageSettings: _*)
  .settings(
    name := "sampler-core"
  )

lazy val arrr = (project in file("sampler-r"))
  .dependsOn(core)
  .settings(commonSettings: _*)
  .settings(packageSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "rengine" % "rengine" % "2015-01-20" from "http://rforge.net/Rserve/files/REngine.jar",
      "rserveengine" % "rserveengine" % "2015-01-20" from "http://rforge.net/Rserve/files/RserveEngine.jar",
      logbackClassic
    )
  )
  .settings(
    name := "sampler-r"
  )

lazy val examples = project.in(file("sampler-examples"))
  .dependsOn(core, abc, arrr)
  .settings(commonSettings: _*)
  .settings(packageSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math3" % "3.2",
      "org.json4s" %% "json4s-native" % "3.2.10",
      logbackClassic,
      commonsIO,
      "org.apache.jclouds.provider" % "aws-ec2" % "1.9.1",
      "org.apache.jclouds.provider" % "softlayer" % "1.9.1"
    )
  )
  .settings(
    name := "sampler-examples",
    mainClass in Compile := Some("sampler.example.abc.UnfairCoin")
  )
  .enablePlugins(JavaAppPackaging)

lazy val spike = project.in(file("sampler-spike"))
  .dependsOn(core, arrr)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math3" % "3.2",
      "org.json4s" %% "json4s-native" % "3.2.11",
      "rengine" % "rengine" % "2015-01-20" from "http://rforge.net/Rserve/files/REngine.jar",
      "rserveengine" % "rserveengine" % "2015-01-20" from "http://rforge.net/Rserve/files/RserveEngine.jar",
      logbackClassic,
      "org.freemarker" % "freemarker" % "2.3.21",
      "org.typelevel" %% "cats" % "0.4.1"
    )
  )
  .settings(
    name := "sampler-spike"
  )

lazy val abc = project.in(file("sampler-abc"))
  .dependsOn(core)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % "2.4.6",
      "com.typesafe.akka" %% "akka-remote" % "2.4.6",
      "com.typesafe.akka" %% "akka-cluster" % "2.4.6",
      "com.typesafe.akka" %% "akka-slf4j" % "2.4.6",
      "com.typesafe.akka" %% "akka-testkit" % "2.4.6" % "test",
      "org.apache.commons" % "commons-math3" % "3.2",
      commonsIO,
      "org.apache.jclouds" % "jclouds-core" % "1.9.2",
      "org.apache.jclouds" % "jclouds-compute" % "1.9.2",
      "org.apache.jclouds.driver" % "jclouds-slf4j" % "1.9.1",
      "com.typesafe.play" %% "play-json" % "2.4.6",
      "com.github.scopt" %% "scopt" % "3.3.0"
    )
  )
  .settings(
    name := "sampler-abc"
  )
