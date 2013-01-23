package sampler.r

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.data.Empirical._
import java.nio.file.Paths
import scala.io.Source
import java.io.File

@RunWith(classOf[JUnitRunner])
class QuickPlotSpec extends Specification {

  // TODO investigate the possibility of mocking ScriptRunner to improve test coverage
  
  "QuickPlot should produce a script for a graph of" in {
    
    "multiple densities from a map of EmpiricalSeq[Double]s" in {
    	val homePath = Paths.get("", "examples", "r")
    	val fileName = "TwoDists"
    			
    	val emp1 = IndexedSeq(0.1,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.5).toEmpiricalSeq
    	val emp2 = IndexedSeq(0.3,0.4,0.4,0.5,0.5,0.5,0.6,0.6,0.7).toEmpiricalSeq
    	
    	val map = Map("Lower" -> emp1, "Higher" -> emp2)
    	
    	QuickPlot.writeDensity(homePath, fileName, map)
    	
      val writtenScript = Source.fromFile(new File(homePath.resolve(fileName).toString)).mkString
      
      val expectedScript =
"""setwd("/home/user/workspace/Sampler/examples/r")
require(ggplot2)
require(reshape)
pdf("TwoDists.pdf", width=8.27, height=5.83)
data <- read.csv("TwoDists.csv")
melted = melt(data)
ggplot(melted, aes(x=value, colour=variable)) + geom_density()
dev.off()"""
      
        val writtenLines = writtenScript.split("\n")
        val expectedLines = expectedScript.split("\n")
        
        def equal(i: Int) = expectedLines(i) mustEqual writtenLines(i)
        
      (0 until expectedLines.length).foreach(equal(_))
    }
  }
}