package sampler.examples.prevchange

import sampler.io.CSVTableWriter
import sampler.r.ScriptRunner
import sampler.data.Types._
import sampler.data.WeightsTable
import sampler.math.Probability
import sampler.data.FrequencyTable
import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

/*
 * Draw posterior distributions and confidence limits for the number of infected 
 * units in a population of size 60, assuming a uniform prior and samples (without 
 * replacement) of size 20
 */
object Posteriors extends App with WithoutReplacementABCModel with Environment{
	val populationSize = 60
	val sampleSize = 20
	val numPositiveObservations = 3
	import model._
	
	val potentialObservations = List(0,1,2,3,5,10,15,17,18,19,20)
	val posteriors = potentialObservations.map{i => getPosterior(i).discardWeights}
	val dataColumns = posteriors
			.zip(potentialObservations)
			.map{case (p, i) => Column(p.samples.map{_.numInfected}, "_"+i.toString)}
	
	@tailrec
	def addZeroIfMissing(map: Map[Int, Int], keyRange: Seq[Int]): Map[Int, Int] = {
		if(keyRange.size == 0) map
		else{
			val newMap = if(!map.contains(keyRange.head)) map + (keyRange.head -> 0) else map
			addZeroIfMissing(newMap, keyRange.tail)
		}
	}
	
	val posteriorCounts = posteriors
			.map{posterior =>
				posterior.samples
					.map{_.numInfected}
					.groupBy(identity)
					.mapValues(_.size)
			}
			.map(counts => addZeroIfMissing(counts, 0 to populationSize))
			.map(counts => TreeMap(counts.toSeq: _*))
			.map(_.iterator.map(_._2))
	val columns = Column(0 to populationSize, "TruePositives") +: 
		posteriorCounts.zip(potentialObservations).map{case (counts, observedPos) =>
			Column(counts.toSeq, observedPos.toString)
		}
	
	new CSVTableWriter(workingDir.resolve("posteriors.csv"), true)(
		columns: _*
	)

	val posteriorPlotScript = 
"""
require(ggplot2)
require(reshape)
			
posteriors = read.csv("posteriors.csv")
			
pdf("posteriors.pdf", width=8.27, height=5.83)
ggplot(melt(posteriors, id="TruePositives"), aes(x=TruePositives, y=value, colour=variable)) +
	geom_line(adjust=1) +
	scale_x_continuous(name="True Num Positives") +
	scale_y_continuous(name="Density") +
	scale_colour_discrete(name="Num Positive Obs.")
dev.off()
"""
	ScriptRunner(posteriorPlotScript, workingDir.resolve("posteriors.r"))
	
	// Plot some confidence limits versus potential observations
	case class Confidence(lower: Double, upper: Double)
	def getConfidence(posterior: FrequencyTable[Parameters]) = {
		val left = posterior.quantile(Probability(0.025))
		val right = posterior.quantile(Probability(0.975))
		Confidence(left.numInfected, right.numInfected)
	}
	
	val results = posteriors.map{getConfidence}
	
	new CSVTableWriter(workingDir.resolve("confidence.csv"), true)(
		Column(potentialObservations, "numPositiveObserved"),
		Column(results.map(_.lower), "lower"),
		Column(results.map(_.upper), "upper")
	)
	
	//Plot a graph in R
	val confidencePlotScript = 
"""
require(ggplot2)
require(reshape)
			
values = read.csv("confidence.csv")
pdf("confidence.pdf", width=8.27, height=5.83)
ggplot(melt(values, id="numPositiveObserved"), aes(x=numPositiveObserved, colour=variable, y=value)) + 
	geom_line() +
	scale_x_continuous(name="Num Positives Observed") +
	scale_y_continuous(name="True Num Positives") +
	scale_colour_discrete(name="95% Confidence Limits")
dev.off()
"""
	ScriptRunner(confidencePlotScript, workingDir.resolve("confidence.r"))
	
}

	