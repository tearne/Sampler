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

package sampler.r.process

import sampler.Implicits._
import sampler.r.process.QuickPlot._
import java.nio.file.Paths
import scala.io.Source
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import QuickPlot._
import org.scalatest.FreeSpec
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import org.scalatest.BeforeAndAfter

class QuickPlotTest 
		extends FreeSpec 
		with MockitoSugar 
		with BeforeAndAfter 
		with ToNamedSeq {
  
  val fileName: String = "plot"
  
  val parentPath: Path = Paths.get(getClass.getClassLoader.getResource("data").toURI())
  
  val pdfPath: Path = parentPath.resolve(fileName + ".pdf")
  val scriptPath: Path = parentPath.resolve(fileName + ".r")
  
  val width: String = "8.27"
  val height: String = "5.83"
  
  after {
		Files.deleteIfExists(scriptPath)
    Files.deleteIfExists(parentPath.resolve(fileName + ".csv"))
    Files.deleteIfExists(parentPath.resolve(fileName + ".Rout"))
    Files.deleteIfExists(pdfPath)
  }
  
  private def discreteScript(fileName: String): String = {
s"""require(ggplot2)
require(reshape)
pdf("$fileName.pdf", width=8.27, height=5.83)
data <- read.csv("$fileName.csv")
ggplot(data, aes(x=value, fill = variable)) + geom_bar(position="dodge")
dev.off()
"""
  }
  
  private def densityScript(fileName: String): String = {
s"""require(ggplot2)
require(reshape)
pdf("$fileName.pdf", width=8.27, height=5.83)
data <- read.csv("$fileName.csv")
ggplot(data, aes(x=value, colour=variable)) + geom_density() + scale_x_continuous(limits=c(0,1))
dev.off()
"""
  }
  
  "Should write single discrete distribution" in {
		val seq = IndexedSeq(1,2,2,3,3,3,4,4,5)
			 
		val instance = new QuickPlot{val runner = mock[ScriptRunner]}
		
		instance.writeDiscrete(pdfPath, width, height, seq.discrete("Integers"))

		val expected = discreteScript(fileName)
		verify(instance.runner).apply(expected, scriptPath)
		
		fail("Haven't tested output csv data")
  }
  
  "Should write multiple discretes distributions" in {
		val seq1 = IndexedSeq(1,2,2,3,3,3,4,4,5)
		val seq2 = IndexedSeq(3,4,4,5,5,5,6,6,7)
				  
		val instance = new QuickPlot{val runner = mock[ScriptRunner]}
		instance.writeDiscrete(pdfPath, width, height, seq1.discrete("s1"), seq2.discrete("s2"))
		
    val expected = discreteScript(fileName)
    verify(instance.runner).apply(expected, scriptPath)
    
    fail("Haven't tested output csv data")
  }
  
  "Should write a single distribution" in {
    val seq = IndexedSeq(0.1,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.5)
    
    val instance = new QuickPlot{val runner = mock[ScriptRunner]}
    instance.writeDensity(pdfPath, width, height, seq.continuous("Doubles"))
    
    val expected = densityScript(fileName)
    verify(instance.runner).apply(expected, scriptPath)
    
    fail("Haven't tested output csv data")

  }
  
  "Should write multiple distributions" in {
    val seq1 = IndexedSeq(0.1,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.5)
    val seq2 = IndexedSeq(0.3,0.4,0.4,0.5,0.5,0.5,0.6,0.6,0.7)
    	
    val instance = new QuickPlot{val runner = mock[ScriptRunner]}
    instance.writeDensity(pdfPath, width, height, seq1.continuous("s1"), seq2.continuous("s2"))
    	
    val expected = densityScript(fileName)
    verify(instance.runner).apply(expected, scriptPath)
    
    fail("Haven't tested output csv data")
  }
}
