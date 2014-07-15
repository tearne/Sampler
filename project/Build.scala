import sbt._
import Keys._
//import sbtassembly.Plugin._
//import AssemblyKeys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys
import sbtunidoc.Plugin._
import UnidocKeys._

object SamplerBuild extends Build{
	val buildOrganization 	= "org.tearne"
	val buildVersion 	= "0.1.1"
	val buildScalaVersion	= "2.11.1"
	
	lazy val commonSettings = Defaults.defaultSettings ++ Seq(
		organization := buildOrganization,
		version		 := buildVersion,
		scalaVersion := buildScalaVersion,

		scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
//		scalacOptions ++= Seq("-encoding", "UTF-8", "-Xlint", "-deprecation", "-unchecked", "-feature"),
		
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
			"org.scalatest" %% "scalatest" % "2.2.0" % "test",
			"org.mockito" % "mockito-all" % "1.9.0" %"test->default",
			"org.slf4j" % "slf4j-api" % "1.7.6",
			"org.scalaz" %% "scalaz-core" % "7.0.6",
			"com.novocode" % "junit-interface" % "0.10" % "test"
		)
	)
		
	lazy val root = project.in(file("."))
		.aggregate(core, examples, cluster)
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
	
	lazy val examples = project.in(file("sampler-examples"))
		.dependsOn(core, cluster)
		.settings(commonSettings: _*)
		//.settings(assySettings: _*)
		.settings(packageSettings: _*)
		.settings(
			libraryDependencies ++= Seq(
				"org.apache.commons" % "commons-math3" % "3.2",
				"ch.qos.logback" % "logback-classic" % "1.1.1"
			)	
		)
		.settings(
			name := "sampler-examples"
		)
		
	lazy val spike = project.in(file("sampler-spike"))
		.dependsOn(core)
		.settings(commonSettings: _*)
		.settings(
			libraryDependencies ++= Seq(
				"org.apache.commons" % "commons-math3" % "3.2"
			)	
		)
		.settings(
			name := "sampler-spike"
		)
	
	lazy val cluster = project.in(file("sampler-cluster"))
		.dependsOn(core)
		.settings(commonSettings: _*)
		.settings(
			libraryDependencies ++= Seq(
				"com.typesafe.akka" %% "akka-actor" % "2.3.3", 
				"com.typesafe.akka" %% "akka-remote" % "2.3.3",
				"com.typesafe.akka" %% "akka-cluster" % "2.3.3",
				"com.typesafe.akka" %% "akka-slf4j" % "2.3.3", 
				"com.typesafe.akka" %% "akka-testkit" % "2.3.3" % "test",
				"org.apache.commons" % "commons-math3" % "3.2",
				"com.amazonaws" % "aws-java-sdk" % "1.4.0.1"
			)	
		)
		.settings(
			name := "sampler-cluster"
		)
	
	
//	val assySettings = assemblySettings ++ Seq(
//		test in assembly := {},
//		mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) => {
//			case "application.conf" => MergeStrategy.discard
//			case "logback.xml" => MergeStrategy.discard
//			case x => old(x)
//		}}
//	)
	
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

	override lazy val settings = super.settings :+ (
		EclipseKeys.skipParents := false		
	)		
}
