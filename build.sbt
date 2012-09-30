name := "Sampler"

version := "0.0.3"

organization := "ahvla"

resolvers ++= Seq(
	"Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
	"Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
	"Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
	"junit" % "junit" % "4.8" % "test->default",
	"org.specs2" %% "specs2" % "1.12.1" % "test",
	"org.mockito" % "mockito-all" % "1.9.0" %"test->default",
	"com.typesafe" % "config" % "0.4.1",
	"com.typesafe.akka" % "akka-actor" % "2.0.2", 
	"com.typesafe.akka" % "akka-remote" % "2.0.2",
	"com.chuusai" %% "shapeless" % "1.2.2",
	"org.apache.commons" % "commons-math3" % "3.0"
)

scalaVersion := "2.9.2"

scalacOptions += "-Ydependent-method-types"