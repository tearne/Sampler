/*
 * Copyright (c) 2012-15 Crown Copyright
 * Animal & Plant Health Agency
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

package sampler.r.script

import java.nio.file.Path

import sampler.io.CSV

import scala.language.implicitConversions

trait QuickPlot{
	val runner: RScript
	import QuickPlot._

	 /** Produces and writes to disk a pdf bar chart for one or more sets of data
   *  
   *  @param path File path (including file name and extension) where the pdf plot is to be written
   *  @param data The data set(s) to be plotted
   */
  def writeDiscrete[T: Integral](filePath: Path, width:String, height:String, data: NamedSeqIntegral[T]*) = {
    val header = Seq[Any]("variable", "value")
    		
    val parentPath = filePath.getParent()
    val pdfFile = filePath.getFileName()
    val fileName = pdfFile.toString.substring(0, pdfFile.toString.lastIndexOf('.'))
    
    def melted(data: Seq[NamedSeqIntegral[T]]): Seq[Seq[Any]] = {
		  data.flatMap{case NamedSeqIntegral(distSeq, name) => distSeq.map{value => Seq(name,value)}}
		}
    
    CSV.writeLines(parentPath.resolve(fileName+".csv"),header +: melted(data))

    val line1 = "data <- read.csv(\"" + fileName + ".csv\")\n"
    val line2 = "ggplot(data, aes(x=value, fill = variable)) + geom_bar(position=\"dodge\")\n"

    val rScript = buildScript(pdfFile, width, height, line1, line2)
	    
		runner(rScript, parentPath.resolve(fileName + ".r"))
  }
  
    /** Produces and writes to disk a pdf density plot showing smooth density estimates for 
   *  one or more sets of data
   *  
   *  @param path File path (including file name and extension) where the pdf plot is to be written
   *  @param data The data set(s) to be plotted
   */
  def writeDensity[T: Fractional](filePath: Path, width:String, height:String, data: NamedSeqFractional[T]*) = {
		val header = Seq[Any]("variable", "value")
		
		val parentPath = filePath.getParent()
		val pdfFile = filePath.getFileName()
		val fileName = pdfFile.substring(0, pdfFile.lastIndexOf('.'))
		
		def melted(data: Seq[NamedSeqFractional[T]]): Seq[Seq[Any]] = {
		  data.flatMap{case NamedSeqFractional(distSeq, name) => distSeq.map{value => Seq(name,value)}}
		}
		
		CSV.writeLines(parentPath.resolve(fileName+".csv"), header +: melted(data))
		  
		val line1 = "data <- read.csv(\"" + fileName + ".csv\")\n"
		val line2 = "ggplot(data, aes(x=value, colour=variable)) + geom_density() + scale_x_continuous(limits=c(0,1))\n"
		    
		val rScript = buildScript(pdfFile, width, height, line1, line2)
		    
		runner(rScript, parentPath.resolve(fileName + ".r"))
  }
}

object QuickPlot extends QuickPlot {
	val runner = RScript
	
  private implicit def pathToString(p:Path) = p.toString()
  
  private def buildScript(fileName: String, width:String, height:String, lines: String*) = {
    val builder = new StringBuilder
    builder.append("require(ggplot2)\n")
		builder.append("require(reshape)\n")
	      
		builder.append("pdf(\"" + fileName + "\", width=" + width + ", height=" + height + ")\n")
		
		lines.foreach(builder.append(_))
		
		builder.append("dev.off()\n")
		      
		builder.toString
  }
}