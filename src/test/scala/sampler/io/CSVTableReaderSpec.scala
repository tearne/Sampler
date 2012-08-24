package sampler.io

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import java.nio.file.Paths
import java.nio.file.Files
import java.io.File
import org.specs2.specification.Scope
import sampler.data.TableHeader

@RunWith(classOf[JUnitRunner])
class CSVTableReaderSpec extends Specification{
	
	"CSVTableReader" should {
		"throw an exception" in {
			"the path points to a file that does not exist" in new fileSetup {
				val nonExistentPath = testPath.resolve("thisFileDoesntExist.csv")
				new CSVTableReader(nonExistentPath) must throwA[RuntimeException]
			}
		}
		
		"retrieve the correct data" in new fileSetup {
			val reader = new CSVTableReader(filePath)
			
			val header1 = new TableHeader("P1")
			
			reader.get(header1)
			
			false
		}
	}
	
	trait fileSetup extends Scope {
		val path = Paths.get(new File("").getAbsolutePath())
		val testPath = path.resolve("testData")
		val filePath = testPath.resolve("testTable.csv")
	}
}