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
  
  /** Allows a sequence of continuous data to be associated with a name, for use when plotting 
   *  
   *  @param dist Sequence of continuous data
   *  @param name Name describing the data
   */
  case class NamedDistribution[T: Fractional](dist: Seq[T], name: String)

  /** Allows a sequence of discrete data to be associated with a name, for use when plotting
  *  
  *  @param dist Sequence of discrete data
  *  @param name Name describing the data
  */
  case class NamedDiscrete[T: Integral](dist: Seq[T], name: String)
  
  /** Implicit conversion from Sequence to NamedDistribution */
  implicit class RichDistribution[T: Fractional](val dist: Seq[T]) {
    /** Creates a new NamedDistribution with the given data and name
     *  
     *  @param name The name associated with the sequence of data
     *  @return a new NamedDistribution with the supplied data and name
     */
    def continuousVariable(name: String) = NamedDistribution(dist, name)
  }
  
  /** Implicit conversion from Sequence to NamedDiscrete */
  implicit class RichDiscrete[T: Integral](val dist: Seq[T]) {
	/** Creates a new NamedDiscrete with the given data and name
	 *  
	 *  @param name The name associated with the sequence of data
	 *  @return a new NamedDiscrete with the supplied data and name
	 */
    def discreteVariable(name: String) = NamedDiscrete(dist, name)
  }
  
  implicit def Seq2NamedDistribution(dist: Seq[Double]) = NamedDistribution(dist, "na"+sampler.math.Random.nextInt(1000))
  implicit def Seq2NamedDiscrete(dist: Seq[Int]) = NamedDiscrete(dist, "na"+sampler.math.Random.nextInt(1000))

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
  def writeDensity[T: Fractional](path: Path, fileName: String, data: NamedDistribution[T]*) = {
	val header = Seq("variable", "value")
    import Numeric.Implicits._
	
	def melted(data: Seq[NamedDistribution[T]]) = {
	  data.flatMap{case NamedDistribution(dist, name) => dist.map{name + "," + _.toDouble}}
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
  def writeDiscrete[T: Integral](path: Path, fileName: String, data: NamedDiscrete[T]*) = {
    val header = Seq("variable", "value")
    
    def melted(data: Seq[NamedDiscrete[T]]) = {
	  data.flatMap{case NamedDiscrete(dist, name) => dist.map{name + "," + _}}
	}
    
    CSVFile.write(path.resolve(fileName+".csv"), melted(data), false, true, header)

    val line1 = "data <- read.csv(\"" + fileName + ".csv\")\n"
    val line2 = "ggplot(data, aes(x=value, fill = variable)) + geom_bar(position=\"dodge\")\n"

    val rScript = buildScript(fileName.toString, line1, line2)
	    
	ScriptRunner(rScript, path.resolve(fileName))
  }
}