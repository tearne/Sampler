/*
 * Copyright (c) 2012 Crown Copyright 
 *                    Animal Health and Veterinary Laboratories Agency
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sampler.r

import sampler.Implicits._
import java.nio.file.Paths
import scala.io.Source
import java.io.File
import sampler.math._
import java.nio.file.Files
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Before
import org.junit.Test
import java.nio.file.Path
import sampler.r.QuickPlot._
import org.scalatest.Matchers

class QuickPlotTest extends AssertionsForJUnit with Matchers {
  
  implicit val r: Random = Random
  
  val fileName: String = "plot"
  
  val parentPath: Path = Paths.get("src", "test", "resources", "data")
  val pdfPath: Path = parentPath.resolve(fileName + ".pdf")
  val scriptPath: Path = parentPath.resolve(fileName)
  
//  @Before def initialise {
//    r = Random
//    
//    fileName = "plot"
//    
//    parentPath = Paths.get("src", "test", "resources", "data")
//    pdfPath = parentPath.resolve(fileName + ".pdf")
//    scriptPath = parentPath.resolve(fileName)
//  }

  def linesTheSame(i: String, j: String) = assert(i === j)
  
  private def discreteScript(name: String): Array[String] = {
    val script =
"""setwd("/home/user/Sampler/sampler-core/src/test/resources/data")
require(ggplot2)
require(reshape)
pdf("""" + name + """.pdf", width=8.27, height=5.83)
data <- read.csv("""" + name + """.csv")
ggplot(data, aes(x=value, fill = variable)) + geom_bar(position="dodge")
dev.off()"""
      
    script.split("\n")
  }
  
  private def densityScript(name: String): Array[String] = {
	val script =
"""setwd("/home/user/Sampler/sampler-core/src/test/resources/data")
require(ggplot2)
require(reshape)
pdf("""" + name + """.pdf", width=8.27, height=5.83)
data <- read.csv("""" + name + """.csv")
ggplot(data, aes(x=value, colour=variable)) + geom_density()
dev.off()"""
				  
	script.split("\n")
  }
  
  @Test def writesSingleDiscreteWithName {
	val seq = IndexedSeq(1,2,2,3,3,3,4,4,5)
			  
	QuickPlot.writeDiscrete(pdfPath, seq.discrete("Integers"))

	val writtenLines = Source.fromFile(new File(scriptPath.toString)).mkString.split("\n")
    val expectedLines = discreteScript(fileName)
    
    deleteRfiles(fileName)

    (0 until expectedLines.length).foreach(i => linesTheSame(writtenLines(i), expectedLines(i)))
  }
  
  @Test def writesMultipleDiscretes {
	val seq1 = IndexedSeq(1,2,2,3,3,3,4,4,5)
	val seq2 = IndexedSeq(3,4,4,5,5,5,6,6,7)
			  
	QuickPlot.writeDiscrete(pdfPath, seq1.discrete("s1"), seq2.discrete("s2"))
	
	val writtenLines = Source.fromFile(new File(scriptPath.toString)).mkString.split("\n")
    val expectedLines = discreteScript(fileName)
    
    deleteRfiles(fileName)

    (0 until expectedLines.length).foreach(i => linesTheSame(writtenLines(i), expectedLines(i)))	  
  }
  
  @Test def writesSingleDistributionWithName {
    val seq = IndexedSeq(0.1,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.5)
    
    QuickPlot.writeDensity(pdfPath, seq.continuous("Doubles"))
    
    val writtenLines = Source.fromFile(new File(scriptPath.toString)).mkString.split("\n")
    val expectedLines = densityScript(fileName)
    
    deleteRfiles(fileName)

    (0 until expectedLines.length).foreach(i => linesTheSame(writtenLines(i), expectedLines(i)))
  }
  
  @Test def writesMultipleDistributions {
    val seq1 = IndexedSeq(0.1,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.5)
    val seq2 = IndexedSeq(0.3,0.4,0.4,0.5,0.5,0.5,0.6,0.6,0.7)
    	
    QuickPlot.writeDensity(pdfPath, seq1.continuous("s1"), seq2.continuous("s2"))
    	
    val writtenLines = Source.fromFile(new File(scriptPath.toString)).mkString.split("\n")
      
    val expectedLines = densityScript(fileName)
        
    deleteRfiles(fileName)

    (0 until expectedLines.length).foreach(i => linesTheSame(writtenLines(i), expectedLines(i)))
  }
  
  private def deleteRfiles(fileName: String) = {
    Files.deleteIfExists(scriptPath)
    Files.deleteIfExists(parentPath.resolve(fileName + ".csv"))
    Files.deleteIfExists(parentPath.resolve(fileName + ".Rout"))
    Files.deleteIfExists(pdfPath)
  }
}
