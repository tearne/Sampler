package sampler.r.process

import java.nio.file.{Paths, Files}
import collection.JavaConversions.asScalaBuffer
import java.nio.file.Path
import org.scalatest.BeforeAndAfter
import org.scalatest.FreeSpec
import scala.io.Source

class ScriptRunnerTest extends FreeSpec with BeforeAndAfter {

  val workingDir = Paths.get(getClass.getResource("").toURI())
  
	val scriptPath = workingDir.resolve("deleteMe.r")
	val rOutPath = workingDir.resolve("deleteMe.r.Rout")
	val jsonPath = workingDir.resolve("deleteMe.json")
	val noExtension = workingDir.resolve("noExtension")
	
	"Prepend script with working dir" in {
  	ScriptRunner("two = 1 + 1", scriptPath)
  	val writtenLines = Source.fromFile(scriptPath.toFile()).mkString.split("\n")
  	val expectedLines = Array(
  			s"""setwd("${workingDir.toAbsolutePath()}")""",
				"two = 1 + 1"
  	)
  	expectedLines.zip(writtenLines).foreach{case (e,a) => assert(e === a)}
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