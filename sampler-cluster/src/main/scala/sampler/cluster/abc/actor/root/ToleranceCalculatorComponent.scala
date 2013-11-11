package sampler.cluster.abc.actor.root

import sampler.math.StatisticsComponent
import sampler.abc.ABCModel
import sampler.math.Probability
import sampler.Implicits._
import akka.actor.ActorLogging

trait ToleranceCalculatorComponent {
	self: StatisticsComponent with ActorLogging=>
		
	val toleranceCalculator: ToleranceCalculator
	
	trait ToleranceCalculator {
		def apply(weighedParameters: Seq[ABCModel#Weighted], fallBackTolerance: Double): Double = {
			val medianMeanScore = statistics.quantile(weighedParameters.map{_.meanScore}.toEmpiricalSeq, Probability(0.5))
					if(medianMeanScore == 0) {
						log.warning("Median of mean scores from last generation evaluated to 0, using fallback tolerance: {}", fallBackTolerance)
						fallBackTolerance
					}
					else math.min(medianMeanScore, fallBackTolerance)
		}
	}
}