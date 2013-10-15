package sampler.r

import sampler.data.Empirical
import java.nio.file.Paths
import java.nio.file.Path
import sampler.data.EmpiricalSeq
import sampler.math.Probability
import sampler.data.EmpiricalTable
import sampler.io.CSVFile
import scala.language.implicitConversions

object QuickPlot {
  private def buildScript(fileName: String, lines: String*) = {
    val builder = new StringBuilder
    builder.append("require(ggplot2)\n")
	builder.append("require(reshape)\n")
	      
	builder.append("pdf(\"" + fileName + ".pdf\", width=8.27, height=5.83)\n")
	
	lines.foreach(builder.append(_))

	builder.append("dev.off()\n")
	      
	builder.toString
  }
  
  /** Produces and writes to disk a pdf density plot showing smooth density estimates for 
   *  one or more sets of data
   *  
   *  @param path The location where the file should be stored
   *  @param fileName The required file name of the pdf plot
   *  @param data The data set(s) to be plotted
   */
  def writeDensity[T: Fractional](path: Path, fileName: String, data: NamedSeqFractional[T]*) = {
	val header = Seq("variable", "value")
    import Numeric.Implicits._
	
	def melted(data: Seq[NamedSeqFractional[T]]) = {
	  data.flatMap{case NamedSeqFractional(dist, name) => dist.map{name + "," + _.toDouble}}
	}
	
	CSVFile.write(path.resolve(fileName+".csv"), melted(data), false, true, header)
	  
	val line1 = "data <- read.csv(\"" + fileName + ".csv\")\n"
	val line2 = "ggplot(data, aes(x=value, colour=variable)) + geom_density()\n"
	    
	val rScript = buildScript(fileName.toString, line1, line2)
	    
	ScriptRunner(rScript, path.resolve(fileName))
  }
  
  /** Produces and writes to disk a pdf bar char for one or more sets of data
   *  
   *  @param path The location where the file should be stored
   *  @param fileName The required file name of the pdf plot
   *  @param data The data set(s) to be plotted
   */
  def writeDiscrete[T: Integral](path: Path, fileName: String, data: NamedSeqIntegral[T]*) = {
    val header = Seq("variable", "value")
    
    def melted(data: Seq[NamedSeqIntegral[T]]) = {
	  data.flatMap{case NamedSeqIntegral(dist, name) => dist.map{name + "," + _}}
	}
    
    CSVFile.write(path.resolve(fileName+".csv"), melted(data), false, true, header)

    val line1 = "data <- read.csv(\"" + fileName + ".csv\")\n"
    val line2 = "ggplot(data, aes(x=value, fill = variable)) + geom_bar(position=\"dodge\")\n"

    val rScript = buildScript(fileName.toString, line1, line2)
	    
	ScriptRunner(rScript, path.resolve(fileName))
  }
}