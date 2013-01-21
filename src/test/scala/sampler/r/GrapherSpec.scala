package sampler.r

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.data.Empirical._
import java.nio.file.Paths
import scala.io.Source
import java.io.File

@RunWith(classOf[JUnitRunner])
class GrapherSpec extends Specification {

  "Grapher should produce a script for a graph of" in {
    
    val homePath = Paths.get("", "examples", "r")
    
    val grapher = new Grapher(homePath)
    
    // TODO modify to have all densities on one plot
    // TODO add second distribution to test and plot name to argument
    
    "multiple densities from a map of EmpiricalSeq[Double]s" in {
    	val emp = IndexedSeq(0.1,0.2,0.2,0.2,0.2,0.3,0.3,0.3,0.3,0.4).toEmpiricalSeq
    	
    	val map = Map("doubles" -> emp)
    	
    	grapher.writeDoubleDensity(map)
    	
      val writtenScript = Source.fromFile(new File(homePath.resolve("scriptDouble").toString)).mkString
      
      val expectedScript =
"""setwd("/home/user/workspace/Sampler/examples/r")
require(ggplot2)
data <- read.csv("doubles.csv")
ggplot(data, aes(x=value)) + geom_density()"""
      
        val writtenLines = writtenScript.split("\n")
        val expectedLines = expectedScript.split("\n")
        
        def equal(i: Int) = expectedLines(i) mustEqual writtenLines(i)
        
      (0 until expectedLines.length).foreach(equal(_))
    }
    
  }
}