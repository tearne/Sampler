package sampler.columnbasedtable

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.matchers.ShouldMatchers
import org.junit.Before
import org.junit.After
import java.nio.file.Paths
import java.nio.file.Path
import org.junit.Test
import java.io.FileNotFoundException
import sampler.math.Probability

class CSVTableReaderTest extends AssertionsForJUnit with ShouldMatchers {

  var testPath: Path = _
  var filePath: Path = _
  var instance: CSVTableReader = _
  
  @Before def initialise {
    testPath = Paths.get("src", "test", "resources", "data")
	filePath = testPath.resolve("testTable.csv")
	instance = new CSVTableReader(filePath)
  }
  
  @Test def readsInDoubles {
    val header = new Header[Double]("MyDoubles")
	assert(instance.get(header) === Column(Seq(1.0, 2.0, 3.0), "MyDoubles"))
  }
  
  @Test def readsInBooleans {
    val header = new Header[Boolean]("TheirBools")
	assert(instance.get(header) === Column(Seq(true, false, false), "TheirBools"))
  }
  
  @Test def readsInStrings {
    val header = new Header[String]("Strings")
	assert(instance.get(header) === Column(Seq("A", "ListOf", "Strings"), "Strings"))
  }
  
  @Test def readsInIntegers {
    val header = new Header[Int]("MyInts")
    assert(instance.get(header) === Column(Seq(2,3,6), "MyInts"))
  }
  
  @Test def readsInFactors {
    val header = new Header[Factor]("SomeFactors")
	
    val c1 = Column(Seq("Factor1", "AnotherFactor", "A_Third").map(Factor(_)), "SomeFactors")
    
    assert(instance.get(header) === c1)
  }
  
  @Test def readsInProbabilities {
    val header = new Header[Probability]("Probs")

    val c1 = Column(Seq(0.789, 0.2, 0.64).map(Probability(_)), "Probs")
    
    assert(instance.get(header) === c1)
  }
  
  @Test def throwExceptionWhenFileDoesntExist {
    val nonExistentPath = testPath.resolve("thisFileDoesntExist.csv")
	
    intercept[FileNotFoundException] {
      new CSVTableReader(nonExistentPath)
    }
  }
  
  @Test def throwExceptionIfHeaderDoesntExist {
    val header = new Header[Double]("DoesntExist")
	
    intercept[UnsupportedOperationException]{
      instance.get(header)
    }
  }
  
  @Test def throwExceptionIfDataIsUnparsable {
    val header = new Header[Double]("DuffDoubles")
	intercept[TableReaderException]{
      instance.get(header)
    }
  }
  
  //			TODO fix quote removal
  @Test def testWillFailBecauseOfQuote {
    val header = new Header[String]("ToFail")
	assert(instance.get(header) === Column(Seq("NoQuote", "With\"Quote", "NoQuote"), "ToFail"))
  }
}