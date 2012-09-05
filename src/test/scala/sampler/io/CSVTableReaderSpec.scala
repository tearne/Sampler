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
import java.io.FileNotFoundException

@RunWith(classOf[JUnitRunner])
class CSVTableReaderSpec extends Specification{
	
	"CSVTableReader" should {
		"throw an exception" in {
			"the path points to a file that does not exist" in new fileSetup {
				val nonExistentPath = testPath.resolve("thisFileDoesntExist.csv")
				new CSVTableReader(nonExistentPath) must throwA[FileNotFoundException]
			}

			"requested a header which doens't exist" in new fileSetup {
				val header = new Header[Double]("DoesntExist")
				instance.get(header) must throwA[UnsupportedOperationException]
			}
			
			"data in table is unparsable / has the wrong type" in new fileSetup {
				val header = new Header[Boolean]("DuffDoubles")
				instance.get(header) must throwA[TableReaderException]
			}
		}
		
		"retrieve data from file when" in {
			"when data type is double" in new fileSetup {
				val header = new Header[Double]("MyDoubles")
				instance.get(header) mustEqual Column(
						Seq(1.0, 2.0, 3.0), 
						"MyDoubles"
				)
			}
			
			"when data type is boolean" in new fileSetup {
				val header = new Header[Boolean]("TheirBools")
				instance.get(header) mustEqual Column(
						Seq(true, false, false), 
						"TheirBools"
				)
			}
			
			"when data type is string" in new fileSetup {
				val header = new Header[String]("Strings")
				instance.get(header) mustEqual Column(
						Seq("A", "ListOf", "Strings"), 
						"Strings"
				)
			}
			
			"when data type is int" in new fileSetup {
				val header = new Header[Int]("MyInts")
				instance.get(header) mustEqual Column(
						Seq(2,3,6), 
						"MyInts"
				)
			}
			
			"when data type is factor" in new fileSetup {
				val header = new Header[Factor]("SomeFactors")
				instance.get(header) mustEqual Column(
						Seq(
							Factor("Factor1"), 
							Factor("AnotherFactor"), 
							Factor("A_Third")
						), 
						"SomeFactors"
				)
			}
			
			"when data type is probability" in new fileSetup {
				val header = new Header[Probability]("Probs")
				instance.get(header) mustEqual Column(
						Seq(
							Probability(0.789), 
							Probability(0.2), 
							Probability(0.64)
						), 
						"Probs"
				)
			}
		}
		
		"test will fail because of quote" in new fileSetup {
			val header = new Header[String]("ToFail")
			instance.get(header) mustEqual Column(
					Seq("NoQuote", "With\"Quote", "NoQuote"), 
					"ToFail"
			)
//			TODO fix quote removal
		}
	}
	
	trait fileSetup extends Scope {
		val testPath = Paths.get("testData")
		val filePath = testPath.resolve("testTable.csv")
		val instance = new CSVTableReader(filePath)
	}
}