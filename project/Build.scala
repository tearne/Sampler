import sbt._
import Keys._
import akka.sbt.AkkaKernelPlugin
import akka.sbt.AkkaKernelPlugin.{ Dist, outputDirectory, distJvmOptions}
 
object KernelBuild extends Build {
    val Organization = "akka.sample"
    val Version = "0.0.8"
    val ScalaVersion = "2.10.1"
     
    lazy val project = Project(
	    id = "cluster-kernel",
	    base = file("."),
	    settings = defaultSettings ++ AkkaKernelPlugin.distSettings ++ Seq(
		    libraryDependencies ++= Dependencies.helloKernel,
		    distJvmOptions in Dist := "-Xms256M -Xmx1024M",
		    outputDirectory in Dist := file("target/cluster-kernel")
	    )
    )
     
    lazy val buildSettings = Defaults.defaultSettings ++ Seq(
	    organization := Organization,
	    version := Version,
	    scalaVersion := ScalaVersion,
	    crossPaths := false,
	    organizationName := "Typesafe Inc.",
	    organizationHomepage := Some(url("http://www.typesafe.com"))
    )
    
    lazy val defaultSettings = buildSettings ++ Seq(
	    resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
	     
	    // compile options
	    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked"),
	    javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")
	     
	)
}
     
object Dependencies {
    import Dependency._
     
    val helloKernel = Seq(
    akkaKernel, akkaSlf4j, logback
    )
}
     
object Dependency {
    // Versions
    object V {
    	val Akka = "2.1.2"
    }
     
    val akkaKernel = "com.typesafe.akka" %% "akka-kernel" % V.Akka// cross CrossVersion.full
    val akkaSlf4j = "com.typesafe.akka" %% "akka-slf4j" % V.Akka// cross CrossVersion.full
    val logback = "ch.qos.logback" % "logback-classic" % "1.0.9"
}

