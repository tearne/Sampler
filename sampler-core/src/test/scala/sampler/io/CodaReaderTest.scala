package sampler.io

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Matchers
import org.junit.Test
import java.nio.file.Paths

class CodaReaderTest extends AssertionsForJUnit with Matchers {

  val dir = Paths.get("src", "test", "resources", "data")
	
  @Test def readsInCsvFile {
    val fileStem = dir.resolve("testCoda")
    
    val data = CodaReader.apply(fileStem.toString)
    
    val dataA = List(0.1,0.2,0.2,0.2,0.3)
    val dataB = List(0.6,0.7,0.7,0.7,0.8)
    
    assert(data("DataA") === dataA)
    assert(data("DataB") === dataB)
  }
  
  @Test def errorIfFileMismatch {
    val fileStem = dir.resolve("shortCoda")
    
    intercept[AssertionError] {
      CodaReader.apply(fileStem.toString)
    }
  }
  
    //
  //TODO move this to a test
  //
  def main(args: Array[String])={
    //apply("WinBUGS/model/output").foreach(println _)
    
//    val data = apply("WinBUGS/model/output")
//    
//  //Read the chain data and spew out the Se/Sp info in column format
//    val writer = new Writer(new File("data/Scala_out/SeSpChain.csv"))
//    writer.addColumn(data("SeH"), "SeH")
//    writer.addColumn(data("SpH"), "SpH")
//    writer.addColumn(data("SeE"), "SeE")
//    writer.addColumn(data("SpE"), "SpE")
//    writer.write()
  }
}