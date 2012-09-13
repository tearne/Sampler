package sampler.r.anova

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import org.specs2.mutable.After
import java.nio.file.Paths
import java.nio.file.Files
import java.io.FileOutputStream
import java.io.PrintStream

//TODO If specific to Anova then put alongside Anova tests in dedicated package

@RunWith(classOf[JUnitRunner])
class JsonReaderSpec extends Specification{

	val filePath = Paths.get("testJSON.txt")
	
	"JsonReader" should {
		
		"throw exception if file not present" in {
			val nonExistentPath = Paths.get("thisFileDoesntExist.txt")
			val jsonReader = new JsonReader()
			jsonReader.apply(nonExistentPath) must throwA[AnovaJsonReaderException]
		}
		
		"contain the correct ANOVA results for parameter 1" in new fileSetup with fileTearDown {
			val jsonReader = new JsonReader()
			val result = jsonReader.apply(filePath)
			
			val entry = result.paramEntries(0)
			
			val expectedEntry = AnovaEntry("Param1", 1, 24.0, 24.0, 7.78, 0.068)
			
			entry mustEqual expectedEntry
		}

		"contain the correct ANOVA results for parameter 2" in new fileSetup with fileTearDown {
			val jsonReader = new JsonReader()
			val result = jsonReader.apply(filePath)
			
			val entry = result.paramEntries(1)
			
			val expectedEntry = AnovaEntry("Param2", 1, 0.75, 0.75, 0.24, 0.655)
			
			entry mustEqual expectedEntry
		}
	}

	trait fileSetup extends Scope {
		val dataFile = new FileOutputStream(filePath.toString)
		val dataStream = new PrintStream(dataFile)
		
		dataStream.print("{\"params\":[\"Param1\",\"Param2\",\"Residuals\"],\"colNames\":[\"Df\",\"Sum Sq\",\"Mean Sq\",\"F value\",\"Pr(>F)\"],\"Df\":[1,1,3],\"Sum Sq\":[24,0.75,9.25],\"Mean Sq\":[24,0.75,3.08],\"F value\":[7.78,0.24,\"NA\"],\"Pr(>F)\":[0.068,0.655,\"NA\"]}")
		dataStream.close()
	}

	trait fileTearDown extends After {
		def after = {
				Files.deleteIfExists(filePath)
		}
	}
}

/*

example JSON file in more readable format

"{
\"params\":[\"Param1\",\"Param2\",\"Residuals\"],
\"colNames\":[\"Df\",\"Sum Sq\",\"Mean Sq\",\"F value\",\"Pr(>F)\"],
\"Df\":[1,1,3],
\"Sum Sq\":[24,0.75,9.25],
\"Mean Sq\":[24,0.75,3.08],
\"F value\":[7.78,0.24,\"NA\"],
\"Pr(>F)\":[0.068,0.655,\"NA\"]
}"

 */