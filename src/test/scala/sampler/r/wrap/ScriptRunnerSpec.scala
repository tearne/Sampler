package sampler.r.wrap

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.r.ScriptRunner
import java.nio.file.Paths
import java.nio.file.Files
import org.specs2.mutable.After

@RunWith(classOf[JUnitRunner])
class ScriptRunnerSpec extends Specification {

	val testPath = Paths.get("testScript.txt")
	val testPathRout = Paths.get("testScript.txt.Rout")

	"ScriptRunner" should {
		
		"take at least 2 seconds to run when the R sleep command is used" in new fileTearDown {
			val startTime = System.nanoTime
			
			val scriptRunner = new ScriptRunner
			
			val builder = new StringBuilder
			builder.append("a<-1+1\n")
			builder.append("Sys.sleep(2)\n")
			builder.append("b<-2+2\n")
			
			scriptRunner.apply(builder.toString(), testPath)
			
			val endTime = System.nanoTime()
			
			val runTime: Double = (endTime - startTime) / 1000000000.0
			
			runTime must beGreaterThan(2.0)
		}
		
		"produce a file containing JSON when the RJSON library is used" in new fileTearDown{
			val scriptRunner = new ScriptRunner
			
			val builder = new StringBuilder
			
			builder.append("library(\"rjson\")\n")
			builder.append("a <- c(1,2,3)\n")
			builder.append("df <- as.data.frame(a)\n")
			builder.append("names(df) <- c(\"parameter\")\n")
			builder.append("jsonOut <- toJSON(df)\n")
			builder.append("fileName <- file(\"" + "myJSON.txt" + "\")\n")
			builder.append("writeLines(jsonOut, fileName)\n")
			builder.append("close(fileName)\n")

			scriptRunner.apply(builder.toString(), testPath)
			
//			TODO read in JSON and test contents
//			TODO delete JSON file after test execution
			
			Nil
		}
	}
	
	trait fileTearDown extends After {
		def after = {
				Files.deleteIfExists(testPath)
				Files.deleteIfExists(testPathRout)
		}
	}
}