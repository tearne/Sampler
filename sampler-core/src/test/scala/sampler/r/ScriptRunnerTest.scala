package sampler.r

import java.nio.file.{Paths, Files}
import collection.JavaConversions.asScalaBuffer
import org.scalatest.junit.AssertionsForJUnit
import org.junit.After
import java.nio.file.Path
import org.junit.Before
import org.junit.Test

class ScriptRunnerTest extends AssertionsForJUnit {

	val workingDir = Paths.get("src", "test", "resources", "data")
	val scriptPath = workingDir.resolve("deleteMe.r")
	val rOutPath = workingDir.resolve("deleteMe.r.Rout")
	val jsonPath = workingDir.resolve("deleteMe.json")
	val noExtension = workingDir.resolve("noExtension")
	
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
	def errorWhenFails {
	  intercept[ScriptRunnerException] {
	    ScriptRunner("results <- toJSON(object)", scriptPath)
	  }

	  intercept[ScriptRunnerException] {
		ScriptRunner("library(\"rjson\")\nresults <- toJSON(object)", scriptPath)
	  }
	}
	
	@Test
	def errorWhenFileNameDoesntEndRFileExtension {
	  intercept[AssertionError] {
	    ScriptRunner("a <- 1", noExtension)
	  }
	}
	
	@After
	def fileTearDown {
	  List(scriptPath, rOutPath, jsonPath, noExtension).foreach(Files.deleteIfExists)
	}
}