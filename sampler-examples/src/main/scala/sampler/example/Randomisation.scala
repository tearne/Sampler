package sampler.example

import java.nio.file.{Files, Paths}

import org.apache.commons.io.FileUtils
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import sampler._
import sampler.distribution.Distribution
import sampler.io.Rounding.Roundable
import sampler.maths.Random
import sampler.r.script.RScript

/*
 * Bootstrap on observations to determine the power of a sampling 
 * experiment in terms of observing a difference between treatment
 * groups.  
 * 
 * Additionally, compare the power of rank (Mann–Whitney U test)
 * with a sum statistic
 */
object Randomisation extends App {

	implicit val r = Random
	val wd = Paths.get("results", "Randomisation").toAbsolutePath()
	Files.createDirectories(wd)

	trait Group
	case object Control extends Group
	case object Treatment extends Group
	case class Response(val value: Double, val group: Group)
	object Response{
		implicit def orderByValue = Ordering.by{r: Response => r.value}
	}
	
	val controlObs = IndexedSeq(-2.563111554, 0.137602944, -3.833588568, -4.999003784, -5.010599635, 1.913070835, 2.448825376, -4.257028083, -3.712720119, -1.582605178, 0.610041145, 4.13967034, 1.631807722, -0.42796491, 0.035666579, -5.029770934, 3.246954255, 0.223542528, 3.803082094, -5.342843905, 8.858159774, -6.315339586, 9.584523137, 4.692787541, -0.980056662, -0.175106954, 3.570247137, 0.626695503, -0.411770106, -0.192063359, -1.222091598, -9.05367629, -3.474498646, 4.360297922, -0.664044765, 3.642900198, -2.266528981, 2.503933899, 3.159185662, 5.879731056, 2.691961071, -2.467236093, 1.942253135, -3.086385876, 8.191174117, -8.370596641, -2.455481745)
		.map{Response(_, Control)}
	val treatmentObs = IndexedSeq(13.65608672, -9.771070031, -1.201278583, -5.516289538, -18.59506765, -1.1918794, -0.684629634, -6.65098211, -0.708664358, -5.507504279, 6.453622365, -8.97723389, -5.360219431, -0.270222351, 1.104715823, -0.021076194, -7.144171315)
		.map{Response(_, Treatment)}
	
	object SampleSize{
		val control = 16
		val treatment = 20
		val total = control + treatment
	}
	
	val controlDist = Distribution.uniform(controlObs)
	val treatmentDist = Distribution.uniform(treatmentObs)
	val combinedDist = Distribution.uniform(controlObs ++: treatmentObs)
			
	def rankStatistic(responses: Seq[Response]) = {
		val sorted = responses.sorted.zipWithIndex
		val controlRankSum = sorted.collect{case (Response(_, Control), idx) => idx + 1}.sum
		
		val uControl = controlRankSum - (SampleSize.control*(SampleSize.control + 1.0) / 2.0)
		val uTreatment = SampleSize.control * SampleSize.treatment - uControl
		
		math.max(uControl, uTreatment)
	}
	
	def sumStatistic(responses: Seq[Response]) = 
		math.abs(responses.foldLeft(0.0){
			case (acc, Response(value, Control)) => acc + value
			case (acc, Response(value, Treatment)) => acc - value
		})
	
	def buildSamplingDistribution(
			control: Distribution[Response],
			treatment: Distribution[Response],
			statistic: Seq[Response] => Double) = {
		{for{
			controlOutcomes <- control.until(_.size == SampleSize.control)
			treatmentOutcomes <- treatment.until(_.size == SampleSize.treatment)
		} yield { controlOutcomes ++: treatmentOutcomes }}
			.map(statistic)
	}
	
	case class Results(
			statisticName: String, 
			nullObs: Seq[Double],
			experimentalObs: Seq[Double]
	){
		val nullEmpirical = nullObs.toEmpirical
		
		def powerAtConfidence(confSeq: Seq[Double]): Seq[Double] = {
      nullEmpirical.percentile(confSeq)
				.map{criticalRightValue => 
					experimentalObs.count{e => e > criticalRightValue} / experimentalObs.size.toDouble
				}
		}
	}
		
	val results = Map(
				"Rank" -> rankStatistic _ ,
				"Sum" -> sumStatistic _
		)
		.map{case (statName,statFun) =>
			val experimentalDist = buildSamplingDistribution(
					controlDist, 
					treatmentDist,
					statFun)
					
			val nullDist = buildSamplingDistribution(
					combinedDist.map(_.copy(group = Control)), 
					combinedDist.map(_.copy(group = Treatment)),
					statFun
			)
				
			val nullObs = (1 to 100000).par.map(_ => nullDist.sample).seq
			val experimentObs = (1 to 100000).par.map(_ => experimentalDist.sample).seq
			
			Results(statName, nullObs, experimentObs)
		}
	
	val confidence = 0.8 until 1 by 0.0002
	
	val json = results.map{r => 
		("stat_name" -> r.statisticName) ~
		("observations" -> {
			("null" -> r.nullObs.map(_.decimalPlaces(3))) ~
			("experimental" -> r.experimentalObs.map(_.decimalPlaces(3)))
		}) ~
		("powers" -> {
			("confidence" -> confidence.map(_.decimalPlaces(3))) ~
			("power" -> r.powerAtConfidence(confidence).map(_.decimalPlaces(3)))
		})
	}
	
	FileUtils.writeStringToFile(
			wd.resolve("json.json").toFile(), 
			pretty(render(json)))
	
	RScript("""
	  library(ggplot2)
	  library(rjson)
	  
	  jsonData = fromJSON(file = "json.json")
	  
	  drawPlots = function(statName){
		  statData = Filter(function(x) x$stat_name == statName, jsonData)[[1]]
			obsData = rbind(
				data.frame(Variable = "null", Statistic = statData$observations$null),
				data.frame(Variable = "experimental", Statistic = statData$observations$experimental)
			)
		  ggplot(obsData, aes(x=Statistic, colour = Variable)) + 
		  	geom_density() +
		  	ggtitle(paste(statName, "Statistic Density"))
	  }
	  
	  pdf("plot.pdf", width=8.26, height=2.91)
	  
	  drawPlots("Rank")
	 	drawPlots("Sum")
	  
	  rankPowers = Filter(function(x) x$stat_name == "Rank",jsonData)[[1]]$powers
	  sumPowers = Filter(function(x) x$stat_name == "Sum",jsonData)[[1]]$powers
	  
	  powerData = rbind(
	  	data.frame(rankPowers, Statistic = "Rank"),
	  	data.frame(sumPowers, Statistic = "Sum")
	  )
  	ggplot(powerData, aes(x=confidence, y=power, colour=Statistic)) + 
  		geom_line() +
			ggtitle("Power at Confidence Levels")
	  
	  dev.off()
	  """,
		wd
	)
}
