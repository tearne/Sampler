/*
 * Copyright (c) 2012-13 Crown Copyright 
 *                       Animal Health and Veterinary Laboratories Agency
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

package sampler.cluster.abc.state.component

import sampler.cluster.abc.Scored
import sampler.cluster.abc.Weighted
import sampler.io.Logging
import sampler.math.StatisticsComponent
import sampler.cluster.abc.Model

trait WeigherComponent {
	this: StatisticsComponent =>
	
	val weigher: Weigher
	
	trait Weigher extends Logging{
		//TODO can remove R
		def getWeightOption[P](
				model: Model[P],
				scoredParticle: Scored[P],
				previousParamsWithWeights: Map[P, Double],
				tolerance: Double
		): Option[Weighted[P]] = {
			def getWeight(particle: Scored[P]): Option[Double] = {
				val fHat = particle.repScores.filter(_ < tolerance).size.toDouble / particle.numReps
				val numerator = fHat * model.prior.density(particle.params)
				val denominator = previousParamsWithWeights.map{case (params0, weight) => 
					weight * model.perturbDensity(params0, particle.params)
				}.sum
				if(numerator > 0 && denominator > 0) Some(numerator / denominator)
				else None
			}
			
			getWeight(scoredParticle).map{wt => Weighted(scoredParticle, wt)}
		}
		
		def consolidateToWeightsTable[P](model: Model[P], population: Seq[Weighted[P]]): Map[P, Double] = {
			population
			.groupBy(_.params)
			.map{case (k,v) => (k, v.map(_.weight).sum)}
		}
	}
}