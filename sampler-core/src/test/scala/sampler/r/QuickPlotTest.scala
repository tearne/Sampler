package sampler.r

import sampler.data.Empirical._
import java.nio.file.Paths
import scala.io.Source
import java.io.File
import sampler.math._
import java.nio.file.Files
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.matchers.ShouldMatchers
import org.junit.Before
import org.junit.Test
import java.nio.file.Path
import sampler.r.QuickPlot._

class QuickPlotTest extends AssertionsForJUnit with ShouldMatchers {
  
  implicit var r: Random = _
  var path: Path = _
  
  // TODO consider mocking of script runner
  
  // TODO should QuickPlot delete everything other than the pdf?
  
  // TODO tests need to check output better
  
  @Before def initialise {
    r = Random
    path = Paths.get("src", "test", "resources", "data")
  }

  def linesTheSame(i: String, j: String) = assert(i === j)
  
  private def densityScript(name: String): Array[String] = {
	val script =
"""setwd("/home/user/workspace/Sampler/sampler-core/src/test/resources/data")
require(ggplot2)
require(reshape)
pdf("""" + name + """.pdf", width=8.27, height=5.83)
data <- read.csv("""" + name + """.csv")
ggplot(data, aes(x=value, colour=variable)) + geom_density()
dev.off()"""
				  
	script.split("\n")
  }
  
  private def discreteScript(name: String): Array[String] = {
    val script =
"""setwd("/home/user/workspace/Sampler/sampler-core/src/test/resources/data")
require(ggplot2)
require(reshape)
pdf("""" + name + """.pdf", width=8.27, height=5.83)
data <- read.csv("""" + name + """.csv")
ggplot(data, aes(x=value, fill = variable)) + geom_bar()
dev.off()"""
      
    script.split("\n")
  }
  
  @Test def writesSingleDiscreteWithName {
	val fileName = "namedDisc"
			  
	val seq = IndexedSeq(1,2,2,3,3,3,4,4,5)
			  
	QuickPlot.writeDiscrete(path, fileName, seq.discreteVariable("Integers"))
	
	val writtenLines = Source.fromFile(new File(path.resolve(fileName).toString)).mkString.split("\n")
    val expectedLines = discreteScript(fileName)
    
    deleteRfiles(fileName)

    (0 until expectedLines.length).foreach(i => linesTheSame(writtenLines(i), expectedLines(i)))
  }
  
  @Test def writesSingleDiscreteWithoutName {
	val fileName = "unnamedDisc"
			  
	val seq = IndexedSeq(1,2,2,3,3,3,4,4,5)
			  
	QuickPlot.writeDiscrete(path, fileName, seq)
	
	val writtenLines = Source.fromFile(new File(path.resolve(fileName).toString)).mkString.split("\n")
    val expectedLines = discreteScript(fileName)
    
    deleteRfiles(fileName)

    (0 until expectedLines.length).foreach(i => linesTheSame(writtenLines(i), expectedLines(i)))
  }
  
  @Test def writesMultipleDiscretes {
	val fileName = "twoDisc"
			  
	val seq1 = IndexedSeq(1,2,2,3,3,3,4,4,5)
	val seq2 = IndexedSeq(3,4,4,5,5,5,6,6,7)
			  
	QuickPlot.writeDiscrete(path, fileName, seq1.discreteVariable("s1"), seq2.discreteVariable("s2"))
	
	val writtenLines = Source.fromFile(new File(path.resolve(fileName).toString)).mkString.split("\n")
    val expectedLines = discreteScript(fileName)
    
//    deleteRfiles(fileName)

    (0 until expectedLines.length).foreach(i => linesTheSame(writtenLines(i), expectedLines(i)))	  
  }
  
  @Test def writesSingleDistributionWithName {
    val fileName = "namedDist"
      
    val seq = IndexedSeq(0.1,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.5)
    
    QuickPlot.writeDensity(path, fileName, seq.continuousVariable("Doubles"))
    
    val writtenLines = Source.fromFile(new File(path.resolve(fileName).toString)).mkString.split("\n")
    val expectedLines = densityScript(fileName)
    
    deleteRfiles(fileName)

    (0 until expectedLines.length).foreach(i => linesTheSame(writtenLines(i), expectedLines(i)))
  }
  
  @Test def writesSingleDistributionWithoutName {
    val fileName = "unnamedDist"
      
    val seq = IndexedSeq(0.1,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.5)
    
    QuickPlot.writeDensity(path, fileName, seq)
    
    val writtenLines = Source.fromFile(new File(path.resolve(fileName).toString)).mkString.split("\n")
      
    val expectedLines = densityScript(fileName)
    
    deleteRfiles(fileName)

    (0 until expectedLines.length).foreach(i => linesTheSame(writtenLines(i), expectedLines(i)))
  }
  
  
  // TODO not quite right
  @Test def writesMultipleDistributions {
    val fileName = "TwoDists"
    			
    val seq1 = IndexedSeq(0.1,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.5)
    val seq2 = IndexedSeq(0.3,0.4,0.4,0.5,0.5,0.5,0.6,0.6,0.7)
    	
    QuickPlot.writeDensity(path, fileName, seq1.continuousVariable("s1"), seq2.continuousVariable("s2"))
    	
    val writtenLines = Source.fromFile(new File(path.resolve(fileName).toString)).mkString.split("\n")
      
    val expectedLines = densityScript(fileName)
        
    deleteRfiles(fileName)

     (0 until expectedLines.length).foreach(i => linesTheSame(writtenLines(i), expectedLines(i)))
  }
  
//  @Test def writeOneDist {
//    val fileName = "distribution"
//      
//    val emp1 = IndexedSeq(0.1,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.5)
//    
//    QuickPlot.writeSingleDensity(path, fileName, emp1)
//    
//    // TODO finish test
//  }
//  
//  @Test def writesMeltedScriptForTwoDistributions{
//    
//    val fileName = "TwoDists"
//    			
//    val emp1 = IndexedSeq(0.1,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.5)
//    val emp2 = IndexedSeq(0.3,0.4,0.4,0.5,0.5,0.5,0.6,0.6,0.7)
//    	
//    val map = Map("Lower" -> emp1, "Higher" -> emp2)
//    	
//    QuickPlot.writeDensity(path, fileName, map)
//    	
//    val writtenScript = Source.fromFile(new File(path.resolve(fileName).toString)).mkString
//      
//    val expectedScript =
//"""setwd("/home/user/workspace/Sampler/sampler-core/src/test/resources/data")
//require(ggplot2)
//require(reshape)
//pdf("TwoDists.pdf", width=8.27, height=5.83)
//data <- read.csv("TwoDists.csv")
//melted = melt(data)
//ggplot(melted, aes(x=value, colour=variable)) + geom_density()
//dev.off()"""
//      
//    val writtenLines = writtenScript.split("\n")
//    val expectedLines = expectedScript.split("\n")
//        
//    deleteRfiles(fileName)
//
//    def equal(i: Int) = expectedLines(i) === writtenLines(i)
//    (0 until expectedLines.length).foreach(equal(_))
//  }
  
  private def deleteRfiles(fileName: String) = {
    Files.deleteIfExists(path.resolve(fileName))
    Files.deleteIfExists(path.resolve(fileName + ".csv"))
    Files.deleteIfExists(path.resolve(fileName + ".Rout"))
    Files.deleteIfExists(path.resolve(fileName + ".pdf"))
  }
}
