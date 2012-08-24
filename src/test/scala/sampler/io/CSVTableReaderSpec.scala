package sampler.io

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import java.nio.file.Paths
import java.nio.file.Files
import java.io.File
import org.specs2.specification.Scope
import sampler.data.Header
import sampler.data.Column

@RunWith(classOf[JUnitRunner])
class CSVTableReaderSpec extends Specification{
	
	"CSVTableReader" should {
//		"throw an exception" in {
//			"the path points to a file that does not exist" in new fileSetup {
//				val nonExistentPath = testPath.resolve("thisFileDoesntExist.csv")
//				new CSVTableReader(nonExistentPath) must throwA[RuntimeException]
//			}
//		}
		
		"retrieve double data" in new fileSetup {
			val reader = new CSVTableReader(filePath)
			
			import sampler.data.HeaderImplicits._
			val header = new Header[Double]("MyDoubles")
			
			reader.get(header) mustEqual Column(IndexedSeq(1.0, 2.0, 3.0), Some("MyDoubles"))
		}
		
		"retrieve boolean data" in new fileSetup {
			val reader = new CSVTableReader(filePath)
			
			import sampler.data.HeaderImplicits._
			val header = new Header[Boolean]("TheirBools")
			
			reader.get(header) mustEqual Column(IndexedSeq(true, false, false), Some("TheirBools"))
		}
	}
	
	trait fileSetup extends Scope {
		val path = Paths.get(new File("").getAbsolutePath())
		val testPath = path.resolve("testData")
		val filePath = testPath.resolve("testTable.csv")
	}
}