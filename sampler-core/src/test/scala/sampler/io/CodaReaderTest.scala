package sampler.io

//import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Matchers
import org.junit.Test
import java.nio.file.Paths
import org.scalatest.FunSuite

class CodaReaderTest extends FunSuite with Matchers {

  val dir = Paths.get("src", "test", "resources", "data")
	
  test("readsInCsvFile") {
    val fileStem = dir.resolve("testCoda")
    
    val data = CodaReader.apply(fileStem.toString)
    
    val dataA = List(0.1,0.2,0.2,0.2,0.3)
    val dataB = List(0.6,0.7,0.7,0.7,0.8)
    
    assert(data("DataA") === dataA)
    assert(data("DataB") === dataB)
  }
  
  test("errorIfFileMismatch") {
    val fileStem = dir.resolve("shortCoda")
    
    intercept[AssertionError] {
      CodaReader.apply(fileStem.toString)
    }
  }
}