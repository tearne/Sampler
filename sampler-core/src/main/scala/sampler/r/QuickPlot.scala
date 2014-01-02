package sampler.r

import java.nio.file.Path

import scala.language.implicitConversions

import sampler.io.CSV

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
	val header = Seq[Any]("variable", "value")
    import Numeric.Implicits._
	
	def melted(data: Seq[NamedSeqFractional[T]]): Seq[Seq[Any]] = {
	  data.flatMap{case NamedSeqFractional(distSeq, name) => distSeq.map{value => Seq(name,value)}}
	}
	
	CSV.writeLines(path.resolve(fileName+".csv"), header +: melted(data))
	  
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
    val header = Seq[Any]("variable", "value")
    
    def melted(data: Seq[NamedSeqIntegral[T]]): Seq[Seq[Any]] = {
	  data.flatMap{case NamedSeqIntegral(distSeq, name) => distSeq.map{value => Seq(name,value)}}
	}
    
    CSV.writeLines(path.resolve(fileName+".csv"),header +: melted(data))

    val line1 = "data <- read.csv(\"" + fileName + ".csv\")\n"
    val line2 = "ggplot(data, aes(x=value, fill = variable)) + geom_bar(position=\"dodge\")\n"

    val rScript = buildScript(fileName.toString, line1, line2)
	    
	ScriptRunner(rScript, path.resolve(fileName))
  }
}