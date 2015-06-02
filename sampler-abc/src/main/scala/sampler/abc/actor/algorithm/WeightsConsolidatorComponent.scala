package sampler.abc.actor.algorithm

import sampler.abc.Model
import sampler.abc.Weighted

trait WeightsConsolidatorComponent {
	val weightsConsolidator: WeightsConsolidator
	
	trait WeightsConsolidator{
		def apply[P](model: Model[P], population: Seq[Weighted[P]]): Map[P, Double] = {
				population
				.groupBy(_.params)
				.map{case (k,v) => (k, v.map(_.weight).sum)}
			}
	}
}