package sampler.io

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Before
import java.nio.file.Path
import java.nio.file.Paths
import org.junit.Test
import java.nio.file.Files
import scala.io.Source
import java.io.File
import org.junit.After

class FailingFileTest extends AssertionsForJUnit {

  var path: Path = _
  var fileName: Path = _
  
  @Before def initialise {
    path = Paths.get("src", "test", "resources", "data")
    fileName = path.resolve("failing.csv")
  }
 
  @Test
  def failingFileWriting {
    
    Files.deleteIfExists(fileName)
    
    val header = Seq("Numbers")
    val data1 = Seq("1","2","3")
    val data2 = Seq("1","2")
      
    CSVFile.write(fileName, data1, append = false, overwrite = true, header = header) // delete this line to make test pass
    CSVFile.write(fileName, data2, append = false, overwrite = true, header = header)

    // Second write should overwrite the first meaning the written file should not contain the third element
  
    val writtenLines = Source.fromFile(new File(fileName.toString)).mkString.split("\n")
    
    assert(writtenLines(0) === "Numbers")
    assert(writtenLines(1).toInt === 1)
    assert(writtenLines(2).toInt === 2)
    assert(writtenLines.length === 3)
  }
  
  @After
  def tearDown {
    Files.deleteIfExists(fileName)
  }
}