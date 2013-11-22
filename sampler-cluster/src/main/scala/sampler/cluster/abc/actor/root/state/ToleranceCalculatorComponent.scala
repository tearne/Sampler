package sampler.cluster.abc.actor.root.state

import sampler.math.StatisticsComponent
import sampler.math.Probability
import sampler.Implicits._
import sampler.abc.Weighted
import sampler.io.Logging

trait ToleranceCalculatorComponent {
	self: StatisticsComponent with Logging=>
		
	val toleranceCalculator: ToleranceCalculator
	
	trait ToleranceCalculator {
		def apply[P](weighedParameters: Seq[Weighted[P]], fallBackTolerance: Double): Double = {
			val medianMeanScore = statistics.quantile(weighedParameters.map{_.meanScore}.toEmpiricalSeq, Probability(0.5))
					if(medianMeanScore == 0) {
						log.warn("Median of mean scores from last generation evaluated to 0, using fallback tolerance: {}", fallBackTolerance)
						fallBackTolerance
					}
					else math.min(medianMeanScore, fallBackTolerance)
		}
	}
}