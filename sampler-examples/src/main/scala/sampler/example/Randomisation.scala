package sampler.example

import sampler.data.DistributionBuilder
import sampler.math.Random
import sampler.Implicits.RichIndexedSeq
import sampler.math.Statistics
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import org.rosuda.REngine.Rserve.RConnection
import java.nio.file.Path
import java.nio.file.Paths
import sampler.r.rserve.RServeHelper
import java.nio.file.Files
import sampler.io.Rounding

object Randomisation extends App with Rounding{
	
	implicit val r = Random

	trait Group
	case object Control extends Group
	case object Treatment extends Group
	case class Response(val value: Double, val group: Group)
	object Response{
		implicit def orderByResponse = Ordering.by{r: Response => r.value}
	}
	val control = IndexedSeq(-2.563111554, 0.137602944, -3.833588568, -4.999003784, -5.010599635, 1.913070835, 2.448825376, -4.257028083, -3.712720119, -1.582605178, 0.610041145, 4.13967034, 1.631807722, -0.42796491, 0.035666579, -5.029770934, 3.246954255, 0.223542528, 3.803082094, -5.342843905, 8.858159774, -6.315339586, 9.584523137, 4.692787541, -0.980056662, -0.175106954, 3.570247137, 0.626695503, -0.411770106, -0.192063359, -1.222091598, -9.05367629, -3.474498646, 4.360297922, -0.664044765, 3.642900198, -2.266528981, 2.503933899, 3.159185662, 5.879731056, 2.691961071, -2.467236093, 1.942253135, -3.086385876, 8.191174117, -8.370596641, -2.455481745)
		.map{value => Response(value, Control)}
	val treatment = IndexedSeq(13.65608672, -9.771070031, -1.201278583, -5.516289538, -18.59506765, -1.1918794, -0.684629634, -6.65098211, -0.708664358, -5.507504279, 6.453622365, -8.97723389, -5.360219431, -0.270222351, 1.104715823, -0.021076194, -7.144171315)
		.map{value => Response(value, Treatment)}
	
	assert(control.size == 47 && treatment.size == 17)
	
	object SampleSize{
		val control = 20
		val treatment = 16
		val total = control + treatment
	}
	
	val controlDist = DistributionBuilder.uniform(control)
	val treatmentDist = DistributionBuilder.uniform(treatment)
	val combinedDist = DistributionBuilder.uniform(control ++: treatment)
			
	val randomisationStatistic = {
		for{
			controlOutcomes <- controlDist.until(_.size == SampleSize.control)
			treatmentOutcomes <- treatmentDist.until(_.size == SampleSize.treatment)
		} yield { controlOutcomes ++: treatmentOutcomes }}
			.map{responses => 
				val sorted = responses.sorted.zipWithIndex
				val controlRankSum = sorted.collect{case (Response(_, Control), idx) => idx + 1}.sum
				
				val uControl = controlRankSum - (SampleSize.control*(SampleSize.control + 1.0) / 2.0)
				val uTreatment = SampleSize.control * SampleSize.treatment - uControl
				
				math.max(uControl, uTreatment)
			}
		
	(1 to 10).foreach(_ => println(randomisationStatistic.sample))
		
	val nullDistribution = {
		{for{
			controlOutcomes <- combinedDist.map{r => r.copy(group = Control)}.until(_.size == SampleSize.control)
			treatmentOutcomes <- combinedDist.map{r => r.copy(group = Treatment)}.until(_.size == SampleSize.treatment)
		} yield { controlOutcomes ++: treatmentOutcomes }}
			.map{responses => 
			val sorted = responses.sorted.zipWithIndex
			val controlRankSum = sorted.collect{case (Response(_, Control), idx) => idx + 1}.sum
			
			val uControl = controlRankSum - (SampleSize.control*(SampleSize.control + 1.0) / 2.0)
			val uTreatment = SampleSize.control * SampleSize.treatment - uControl
			
			math.max(uControl, uTreatment)
		}
	}
		
	val nullSamples = (1 to 1000000).map(_ => nullDistribution.sample)
	val nullTable = nullSamples.toEmpiricalTable
	
	val experiments = (1 to 10000).map{_ => randomisationStatistic.sample}
	val rightTails = experiments.map{e => Statistics.rightTail(nullTable, e)}
	val nintyFifthPercentile = Statistics.quantile(nullTable, 0.95)
	println("-----> "+nintyFifthPercentile)
	val meanRightTail = (rightTails.sum / rightTails.size.toDouble).decimalPlaces(3)
	
	println(s"Mean right tail = $meanRightTail")
	
	val json = 
		("rightTails" -> rightTails.map(_.decimalPlaces(3))) ~
		("experiments" -> experiments.map(_.decimalPlaces(3))) ~
		("nullSamples" -> nullSamples.map(_.decimalPlaces(3))) ~
		("ninetyFifthPercentile" -> nintyFifthPercentile)

	val wd = Paths.get("results", "Randomisation").toAbsolutePath()
	Files.createDirectories(wd)
	val writer = Files.newBufferedWriter(wd.resolve("json.json"))
	writer.write(pretty(render(json)))
	writer.close()
//	println(s"""------>setwd("$wd")""")
	
	RServeHelper.ensureRunning()
	val rCon = new RConnection
  try {    
	  rCon.assign("jsonIn", compact(render(json)))
  	rCon.parseAndEval("library(ggplot2)")
  	rCon.parseAndEval("library(rjson)")
  	rCon.parseAndEval(s"""setwd("$wd")""")
  	rCon.parseAndEval("""
  	  pdf("plot.pdf", width=8.26, height=2.91)
  		data = fromJSON(jsonIn)
  		statistics = rbind(
  			data.frame(variable = "null", statistic = data$nullSamples),
  			data.frame(variable = "experiment", statistic = data$experiments)
  		)
  	  print(
  	  	ggplot(statistics, aes(x=statistic, colour = variable)) + 
  	  	geom_density() +
  	  	geom_vline(xintercept = data$ninetyFifthPercentile, colour = 'red')
  	  )
  		
  	  tailsData = data.frame(rightTails = fromJSON(jsonIn)$rightTails)
  	  print(ggplot(tailsData, aes(x=rightTails)) + geom_density())
  	  dev.off()
  	""")
  	
  } finally {
    rCon.close
  }
}