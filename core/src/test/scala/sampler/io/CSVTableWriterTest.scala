package sampler.io

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.matchers.ShouldMatchers
import org.junit.Before
import java.nio.file.Paths
import sampler.io.table.CSVTableWriter
import java.io.File
import org.junit.After
import java.nio.file.Files
import java.nio.file.Path
import sampler.io.table.CSVTableWriter
import org.junit.Test
import sampler.io.table.Column
import scala.io.Source
import java.nio.file.FileAlreadyExistsException
import sampler.io.table.TableWriterException
import sampler.io.table.Factor
import sampler.math.Probability

class CSVTableWriterTest extends AssertionsForJUnit with ShouldMatchers {
  
  // TODO consider sequentially
  
  var filePath: Path = _
  var writer: CSVTableWriter = _
  
  @Before def initialise {
    val path = Paths.get(new File("").getAbsolutePath())
	filePath = path.resolve("src/test/resources/data/testFile.csv")
	writer = new CSVTableWriter(filePath)
  }
  
  @After def tearDown {
    Files.deleteIfExists(filePath)
  }
  
  @Test def writerCreatesNewFile {
	val col = new Column(Seq(1,2,3), "MyInts")
			
	writer.apply(col)
	assert(Files.exists(filePath) === true)
  }
  
  @Test def canWriteIntegers {
	val intCol = new Column(Seq(1,2), "MyInts")
	writer.apply(intCol)
	
	val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
				
	assert(lines === Array("MyInts", "1", "2"))
  }
  
  @Test def canWriteDoubles {
	val doubleCol = new Column(Seq(2.000,1.0), "MyDoubles")
	writer.apply(doubleCol)
	
	val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
				
	assert(lines === Array("MyDoubles", "2.0", "1.0"))
  }
  
  @Test def canWriteStrings {
	val stringCol = new Column(Seq("String", "List"), "MyStrings")
	writer.apply(stringCol)
	
	val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
				
	assert(lines === Array("MyStrings", "String", "List"))
  }
  
  @Test def canWriteFactors {
	val factorCol = new Column(Seq(Factor("F1"), Factor("F2")), "MyFactors")
	writer.apply(factorCol)

	val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
	
	assert(lines ===  Array("MyFactors", "F1", "F2"))
  }
  
  @Test def canWriteBooleans {
	val booleanCol = new Column(Seq(true, false), "MyBools")
	writer.apply(booleanCol)
	
	val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
	
	assert(lines === Array("MyBools", "true", "false"))
  }
  
  @Test def canWriteProbabilities {
	val probCol = new Column(Seq(Probability(0.5), Probability(0.321)), "MyProbs")
    writer.apply(probCol)
	
    val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
    assert(lines === Array("MyProbs", "0.5", "0.321"))
  }
  
  @Test def writerOverwritesExistingFile {
	val writer2 = new CSVTableWriter(filePath, true)
	
	val c1 = new Column(Seq(1,2,3), "Header")
	writer.apply(c1)

	val c2 = new Column(Seq("one", "two", "three"), "Header")
	writer2.apply(c2)
				
	val lines = Source.fromFile(filePath.toString()).mkString.split("\n")
	assert(lines === Array("Header", "one", "two", "three")) 
  }
  
  @Test def doesntOverwriteIfSetToFalse {
    val c1 = new Column(Seq(1,2,3), "Header")
    writer.apply(c1)
				
	val writer2 = new CSVTableWriter(filePath, false)
				
    intercept[FileAlreadyExistsException] {
      writer2.apply(c1)
    }
  }
  
  @Test def defaultsToNoOverwrite {
    val c1 = new Column(Seq(1,2,3), "Header")
    writer.apply(c1)
				
    intercept[FileAlreadyExistsException] {
      writer.apply(c1)
    }
  }
  
  @Test def throwExceptionWhenNoDataSupplied { 
    intercept[TableWriterException] {
      writer.apply()
    }
  }	
  
  @Test def throwExceptionWhenColumnsOfDifferentLength {
    val p1 = Seq(1,2,3)
	val p2 = "Lots and lots and lots of entries".split(" ")
						
	val c1 = new Column(p1, "SomeInts")
	val c2 = new Column(p2, "LoadsaStrings")
				
	intercept[TableWriterException] {
      writer.apply(c1, c2)
    }
  }

  @Test def throwExceptionWhenColumnNameDuplicated {
    val c1 = new Column(Seq(1,2,3), "Name")
	val c2 = new Column(Seq(4,5,6), "Name")
	
    intercept[TableWriterException] {
      writer.apply(c1, c2)
    }
  }
}