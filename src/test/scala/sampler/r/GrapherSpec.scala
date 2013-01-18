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
    
    "distribution density from a single EmpiricalSeq[Int]" in {
      val emp = IndexedSeq(1,2,2,3).toEmpiricalSeq
      
      val empMap = Map("small" -> emp)
      
      grapher.writeIntDensity(empMap)
      
      val writtenScript = Source.fromFile(new File(homePath.resolve("scriptInt").toString)).mkString
      
      val expectedScript =
"""setwd("/home/user/workspace/Sampler/examples/r")
require(ggplot2)
data <- read.csv("small.csv")
ggplot(data, aes(x=value)) + geom_density()"""
      
        val writtenLines = writtenScript.split("\n")
        val expectedLines = expectedScript.split("\n")
        
        def equal(i: Int) = expectedLines(i) mustEqual writtenLines(i)
        
      (0 until expectedLines.length).foreach(equal(_))
    }
    
    "multiple densities from a map of EmpiricalSeq[Int]s" in {
      val emp1 = IndexedSeq(1,2,2,3).toEmpiricalSeq
      val emp2 = IndexedSeq(4,5,5,6,6,6,7,7,8).toEmpiricalSeq
      
      val empMap = Map("small" -> emp1, "bigger" -> emp2)
      
      grapher.writeIntDensity(empMap)
      
      val writtenScript = Source.fromFile(new File(homePath.resolve("scriptInt").toString)).mkString
      
      val expectedScript =
"""setwd("/home/user/workspace/Sampler/examples/r")
require(ggplot2)
data <- read.csv("small.csv")
ggplot(data, aes(x=value)) + geom_density()
data <- read.csv("bigger.csv")
ggplot(data, aes(x=value)) + geom_density()"""
      
        val writtenLines = writtenScript.split("\n")
        val expectedLines = expectedScript.split("\n")
        
        def equal(i: Int) = expectedLines(i) mustEqual writtenLines(i)
        
      (0 until expectedLines.length).foreach(equal(_))
    }
    
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
    
    "distribution density from a map of EmpiricalTable[Int]s" in {
    	todo
    }
    
    "distribution density from a map of EmpiricalTable[Double]s" in {
    	todo
    }    
  }
}