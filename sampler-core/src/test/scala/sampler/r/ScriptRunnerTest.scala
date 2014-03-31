package sampler.r

import java.nio.file.{Paths, Files}
import collection.JavaConversions.asScalaBuffer
import java.nio.file.Path
import org.scalatest.BeforeAndAfter
import org.scalatest.FreeSpec

class ScriptRunnerTest extends FreeSpec with BeforeAndAfter {

  val workingDir = Paths.get(getClass.getClassLoader.getResource("data").toURI())
  
	val scriptPath = workingDir.resolve("deleteMe.r")
	val rOutPath = workingDir.resolve("deleteMe.r.Rout")
	val jsonPath = workingDir.resolve("deleteMe.json")
	val noExtension = workingDir.resolve("noExtension")
	
	"Nothing prints out from simple script" in {
	  val script =
"""
a <- c(2,4,6)
a
"""
	}
	
	"Runs sleep command" in {
	  val startTime = System.nanoTime
	  ScriptRunner("Sys.sleep(1)", scriptPath)
      val runTime = (System.nanoTime() - startTime) / 1e9
			
	  assert(runTime > 1.0)
	}
	
	"ScriptRunnerException when code fails" in {
	  intercept[ScriptRunnerException] {
	    ScriptRunner("results <- toJSON(object)", scriptPath)
	  }

	  intercept[ScriptRunnerException] {
		ScriptRunner("library(\"rjson\")\nresults <- toJSON(object)", scriptPath)
	  }
	}
	
	"Error when file name doesn't end with an R file extension" in {
	  intercept[AssertionError] {
	    ScriptRunner("a <- 1", noExtension)
	  }
	}
	
	after {
	  List(scriptPath, rOutPath, jsonPath, noExtension).foreach(Files.deleteIfExists)
	}
}