package sampler.io

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import java.nio.file.Paths
import java.nio.file.Files
import java.io.File
import org.specs2.specification.Scope
import sampler.data.Types._
import sampler.math.Probability

@RunWith(classOf[JUnitRunner])
class CSVTableReaderSpec extends Specification{
	
	"CSVTableReader" should {
//		"throw an exception" in {
//			"the path points to a file that does not exist" in new fileSetup {
//				val nonExistentPath = testPath.resolve("thisFileDoesntExist.csv")
//				new CSVTableReader(nonExistentPath) must throwA[RuntimeException]
//			}
//		}
		
		"throw an exception" in {
			"requested a header which doens't exist" in new fileSetup {
				val header = new Header[Double]("DoesntExist")
			instance.get(header) must throwA[UnsupportedOperationException]
			}
		}
		
		"retrieve double data" in new fileSetup {
			val header = new Header[Double]("MyDoubles")
			instance.get(header) mustEqual Column(IndexedSeq(1.0, 2.0, 3.0), Some("MyDoubles"))
		}
		
		"retrieve boolean data" in new fileSetup {
			val header = new Header[Boolean]("TheirBools")
			instance.get(header) mustEqual Column(IndexedSeq(true, false, false), Some("TheirBools"))
		}
		
		"retrieve string data" in new fileSetup {
			val header = new Header[String]("Strings")
			instance.get(header) mustEqual Column(IndexedSeq("A", "ListOf", "Strings"), Some("Strings"))
		}
		
		"retrieve int data" in new fileSetup {
			val header = new Header[Int]("MyInts")
			instance.get(header) mustEqual Column(IndexedSeq(2,3,6), Some("MyInts"))
		}
		
		"retrieve factor data" in new fileSetup {
			val header = new Header[Factor]("SomeFactors")
			instance.get(header) mustEqual Column(IndexedSeq(Factor("Factor1"), Factor("AnotherFactor"), Factor("A_Third"))
					, Some("SomeFactors"))
		}
		
		"retrieve probability data" in new fileSetup {
			val header = new Header[Probability]("Probs")
			instance.get(header) mustEqual Column(IndexedSeq(Probability(0.789), Probability(0.2), Probability(0.64))
					, Some("Probs"))
		}
		
		"test will fail" in new fileSetup {
			val header = new Header[String]("ToFail")
			instance.get(header) mustEqual Column(IndexedSeq("NoQuote", "With\"Quote", "NoQuote"), Some("ToFail"))
		}
	}
	
	trait fileSetup extends Scope {
		val path = Paths.get(new File("").getAbsolutePath())
		val testPath = path.resolve("testData")
		val filePath = testPath.resolve("testTable.csv")
		val instance = new CSVTableReader(filePath)
	}
}