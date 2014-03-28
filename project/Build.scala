import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys
import sbtunidoc.Plugin._
import UnidocKeys._

object SamplerBuild extends Build{
	val buildOrganization 	= "org.tearne"
	val buildVersion 	= "0.0.22"
	val buildScalaVersion	= "2.10.3"
	
	lazy val root = Project(
		id = "sampler",
		base = file("."),
		settings = buildSettings ++ unidocSettings ++ Seq(
				unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(spike)
		),
		aggregate = Seq(core, examples, cluster)
	) 
	
	lazy val core = Project(
		id = "sampler-core",
		base = file("sampler-core"),
		settings = buildSettings ++ assySettings ++ packageSettings
	)
	
	lazy val examples = Project(
		id = "sampler-examples",
		base = file("sampler-examples"),
		settings = buildSettings ++ assySettings ++ packageSettings ++ Seq(
			libraryDependencies ++= Seq(
				"org.apache.commons" % "commons-math3" % "3.2"
			)
		)
	) dependsOn(core, cluster)
	
	lazy val spike = Project(
		id = "sampler-spike",
		base = file("sampler-spike"),
		settings = buildSettings ++ Seq(
			libraryDependencies ++= Seq(
				"org.apache.commons" % "commons-math3" % "3.2"
			)
		)
	) dependsOn core
	
	lazy val cluster = Project(
		id = "sampler-cluster",
		base = file("sampler-cluster"),
		settings = buildSettings ++ Seq(
			libraryDependencies ++= Seq(
				"com.typesafe.akka" %% "akka-actor" % "2.3.1", 
				"com.typesafe.akka" %% "akka-remote" % "2.3.1",
				"com.typesafe.akka" %% "akka-cluster" % "2.3.1",
				"com.typesafe.akka" %% "akka-slf4j" % "2.3.1", 
				"com.typesafe.akka" %% "akka-testkit" % "2.3.1" % "test",
				"org.apache.commons" % "commons-math3" % "3.2",
				"com.amazonaws" % "aws-java-sdk" % "1.4.0.1"
			)
		)
	) dependsOn core
	
	val assySettings = assemblySettings ++ Seq(
		test in assembly := {},
		mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) => {
			case "application.conf" => MergeStrategy.discard
			case "logback.xml" => MergeStrategy.discard
			case x => old(x)
		}}
	)
	
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
	
	lazy val buildSettings = Defaults.defaultSettings ++ Seq(
		organization := buildOrganization,
		version		 := buildVersion,
		scalaVersion := buildScalaVersion,

		scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
//		scalacOptions ++= Seq("-encoding", "UTF-8", "-Xlint", "-deprecation", "-unchecked", "-feature"),
		
		//Copies all dependencies to lib_managed
		retrieveManaged	:= true,
		
		resolvers ++= Seq(
			"Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
			"Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
			"Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
		),
		
		libraryDependencies ++= Seq(
			"com.typesafe" % "config" % "1.2.0",
			"junit" % "junit" % "4.8" % "test->default",
			"org.scalatest" % "scalatest_2.10" % "2.1.0" % "test",
			"org.specs2" %% "specs2" % "1.13" % "test",
			"org.mockito" % "mockito-all" % "1.9.0" %"test->default",
			"ch.qos.logback" % "logback-classic" % "1.0.12",
			"org.scalaz" %% "scalaz-core" % "7.0.3"
		)
	)

	override lazy val settings = super.settings :+ (
		EclipseKeys.skipParents := false		
	)		
}
