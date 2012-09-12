package sampler.r

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import java.nio.file.{Paths, Files}
import org.specs2.mutable.After
import com.typesafe.config.ConfigFactory
import collection.JavaConversions.asScalaBuffer

@RunWith(classOf[JUnitRunner])
class ScriptRunnerSpec extends Specification {

	val workingDir = Paths.get("testData")
	val scriptPath = workingDir.resolve("deleteMe.r")
	val rOutPath = workingDir.resolve("deleteMe.r.Rout")
	val jsonPath = workingDir.resolve("deleteMe.json")

	"ScriptRunner" should {
		"block on script execution" in new FileTearDown {
			val startTime = System.nanoTime
			ScriptRunner("Sys.sleep(1)", scriptPath)
			val runTime = (System.nanoTime() - startTime) / 1e9
			
			runTime must beGreaterThan(1.0)
		}
		
		"allow JSON results to be written from R" in new FileTearDown{
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
			
			result mustEqual List(2,4,6)
		}
		
		"throw exception if R script fails" in todo
		//TODO AG: inspect Rout for signs of errors
	}
	
	trait FileTearDown extends After {
		def after = {
			List(scriptPath, rOutPath, jsonPath).foreach(Files.deleteIfExists)
		}
	}
}