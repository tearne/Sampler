package sampler.io

import org.junit.Test
import java.nio.file.Paths
import org.scalatest.FreeSpec

class CodaReaderTest extends FreeSpec {

  val dir = Paths.get(getClass.getClassLoader.getResource("data").toURI())
	
  "Reads in .csv files" in {
    val fileStem = dir.resolve("testCoda")
    
    val data = CodaReader.apply(fileStem.toString)
    
    val dataA = List(0.1,0.2,0.2,0.2,0.3)
    val dataB = List(0.6,0.7,0.7,0.7,0.8)
    
    assert(data("DataA") === dataA)
    assert(data("DataB") === dataB)
  }
  
  "Error if file mismatch" in {
    val fileStem = dir.resolve("shortCoda")
    
    intercept[AssertionError] {
      CodaReader.apply(fileStem.toString)
    }
  }
}