import UnidocKeys._

val buildOrganization 	= "org.tearne"
val buildVersion 	= "0.2.6"
val buildScalaVersion	= "2.11.7"

lazy val commonSettings = Seq(
	organization := buildOrganization,
	version		 := buildVersion,
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
		"org.spire-math" %% "spire" % "0.11.0"
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
	.settings(unidocSettings: _*)
	.settings(
		name := "sampler",
		unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(spike) -- inProjects(examples)
	)
	
lazy val core = project.in(file("sampler-core"))
	.settings(commonSettings: _*)
	//.settings(assySettings: _*)
	.settings(packageSettings: _*)
	.settings(
		name := "sampler-core"
	)
	
lazy val arrr = (project in file("sampler-r"))
	.dependsOn(core)
	.settings(commonSettings: _*)
	.settings(unidocSettings: _*)
	.settings(packageSettings: _*)
	.settings(
		libraryDependencies ++= Seq(
			"rengine" % "rengine" % "2015-01-20" from "http://rforge.net/Rserve/files/REngine.jar",
			"rserveengine" % "rserveengine" % "2015-01-20" from "http://rforge.net/Rserve/files/RserveEngine.jar",
			"ch.qos.logback" % "logback-classic" % "1.1.1"
		)	
	)
	.settings(
		name := "sampler-r"
	)
	
lazy val examples = project.in(file("sampler-examples"))
	.dependsOn(core, abc, arrr)
	.settings(commonSettings: _*)
	//.settings(assySettings: _*)
	.settings(packageSettings: _*)
	.settings(
		libraryDependencies ++= Seq(
			"org.apache.commons" % "commons-math3" % "3.2",
			"org.json4s" %% "json4s-native" % "3.2.10",
			"ch.qos.logback" % "logback-classic" % "1.1.1",
			"org.apache.commons" % "commons-io" % "1.3.2"
		)	
	)
	.settings(
		name := "sampler-examples"
	)
	
lazy val spike = project.in(file("sampler-spike"))
	.dependsOn(core, arrr)
	.settings(commonSettings: _*)
	.settings(
		libraryDependencies ++= Seq(
			"org.apache.commons" % "commons-math3" % "3.2",
			"org.json4s" %% "json4s-native" % "3.2.11",
			"rengine" % "rengine" % "2015-01-20" from "http://rforge.net/Rserve/files/REngine.jar",
			"rserveengine" % "rserveengine" % "2015-01-20" from "http://rforge.net/Rserve/files/RserveEngine.jar",
			"ch.qos.logback" % "logback-classic" % "1.1.1",
			"org.freemarker" % "freemarker" % "2.3.21"
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
			"com.typesafe.akka" %% "akka-actor" % "2.3.6", 
			"com.typesafe.akka" %% "akka-remote" % "2.3.6",
			"com.typesafe.akka" %% "akka-cluster" % "2.3.6",
			"com.typesafe.akka" %% "akka-slf4j" % "2.3.6", 
			"com.typesafe.akka" %% "akka-testkit" % "2.3.6" % "test",
			"org.apache.commons" % "commons-math3" % "3.2",
			"commons-io" % "commons-io" % "2.4",
			"org.apache.jclouds" % "jclouds-core" % "1.9.1",
			"org.apache.jclouds.driver" % "jclouds-slf4j" % "1.9.1",
			"org.apache.jclouds.provider" % "aws-ec2" % "1.9.1",
			"com.jayway.jsonpath" % "json-path" % "2.0.0",
			"com.amazonaws" % "aws-java-sdk" % "1.4.0.1"
		)	
	)
	.settings(
		name := "sampler-abc"
	)


