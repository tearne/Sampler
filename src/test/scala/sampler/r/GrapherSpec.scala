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
      
      grapher.writeDensity(empMap)
      
      val writtenScript = Source.fromFile(new File(homePath.resolve("script").toString)).mkString
      
      val expectedScript =
"""setwd("/home/user/workspace/Sampler/examples/r")
require(ggplot2)
data <- read.csv("small.csv")
ggplot(data, aes(x=value)) + geom_density()"""
      
        val writtenLines = writtenScript.split("\n")
        val expectedLines = expectedScript.split("\n")
        
        def equal(i: Int) = expectedLines(i) mustEqual writtenLines(i)
        
      (0 until writtenLines.length).foreach(equal(_))
    }
    
    "multiple densities from a map of EmpiricalSeq[Int]s" in {
      todo
    }
    
    "multiple densities from a map of EmpiricalSeq[Double]s" in {
    	todo
    }
    
    "distribution density from a map of EmpiricalTable[Int]s" in {
    	todo
    }
    
    "distribution density from a map of EmpiricalTable[Double]s" in {
    	todo
    }
    
//    "test with more complicated empirical table" in {
//      import java.nio.file.Paths
//      import sampler.io.ChainReader
//
//      val pathspec = Paths.get("", "examples", "ncpSampleSize", "data", "coda")
//  
//      val chains = ChainReader(pathspec.toString())
//      
//      val name = "PPosNCPFaecesNonCage[6]"
//      
//      val chain = chains(name)
//      
//      println(name)
//      println("Min: " + chain.min)
//      println("Average: " + chain.sum/chain.length)
//      println("Max: " + chain.max)
//      
//      val emp = chain.toEmpiricalTable
//      
//      val wrapper = new Grapher
//      
//      wrapper.apply(emp)
//    }
  }
}