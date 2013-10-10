package sampler.r

import java.nio.file.{Paths, Files}
import com.typesafe.config.ConfigFactory
import collection.JavaConversions.asScalaBuffer
import org.scalatest.junit.AssertionsForJUnit
import org.junit.After
import java.nio.file.Path
import org.junit.Before
import org.junit.Test

class ScriptRunnerTest extends AssertionsForJUnit {

	var workingDir: Path = _
	var scriptPath: Path = _
	var rOutPath: Path = _
	var jsonPath: Path = _

	@Before
	def initialse {
	  workingDir = Paths.get("src", "test", "resources", "data")
	  scriptPath = workingDir.resolve("deleteMe.r")
	  rOutPath = workingDir.resolve("deleteMe.r.Rout")
	  jsonPath = workingDir.resolve("deleteMe.json")
	}
	
	@Test
	def doesntPrintOut {
	  val script =
"""
a <- c(2,4,6)
a
"""
	}
	
	@Test
	def runsSleepCommand {
	  val startTime = System.nanoTime
	  ScriptRunner("Sys.sleep(1)", scriptPath)
      val runTime = (System.nanoTime() - startTime) / 1e9
			
	  assert(runTime > 1.0)
	}
	
	@Test
	def writesJSONscript {
	  val script =
"""
require("rjson")
a <- c(2,4,6)
df <- data.frame(parameter=a)
jsonOut <- toJSON(df)
fileName <- file("deleteMe.json")
writeLines(jsonOut, fileName)
close(fileName)
"""

	  ScriptRunner(script, scriptPath)
	  val config = ConfigFactory.parseFile(jsonPath.toFile())	//TODO OT: jerkson
	  val result = asScalaBuffer(config.getIntList("parameter"))
			
	  assert(result === List(2,4,6))
	}
	
	@Test
	def errorWhenFails {
	  intercept[ScriptRunnerException] {
	    ScriptRunner("results <- toJSON(object)", scriptPath)
	  }

	  intercept[ScriptRunnerException] {
		ScriptRunner("library(\"rjson\")\nresults <- toJSON(object)", scriptPath)
	  }
	}
	
	@After
	def fileTearDown {
	  List(scriptPath, rOutPath, jsonPath).foreach(Files.deleteIfExists)
	}
}