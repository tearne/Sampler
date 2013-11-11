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

package sampler.cluster.abc.actor.root

import sampler.abc.ABCModel
import sampler.io.Logging
import sampler.math.StatisticsComponent
import sampler.Implicits._

trait WeigherComponent {
	self: StatisticsComponent =>
	
	val weigher: Weigher
	
	trait Weigher extends Logging{
		def filterAndWeighScoredParameterSet(model: ABCModel)(
				newScoredParam: model.Scored,
				previousParamsWithWeights: Map[model.ParameterSet, Double],
				tolerance: Double
				): Option[model.Weighted] = {
			import model._
			def getWeight(scored: Scored): Option[Double] = {
				val fHat = scored.runScores.filter(_ < tolerance).size.toDouble
						val numerator = fHat * prior.density(scored.parameterSet)
						val denominator = previousParamsWithWeights.map{case (params0, weight) => 
						weight * params0.perturbDensity(scored.parameterSet)
				}.sum
				if(numerator > 0 && denominator > 0) Some(numerator / denominator)
				else None
			}
			
			getWeight(newScoredParam).map{wt => Weighted(newScoredParam, wt)}
		}
		
		def consolidateToWeightsTable(model: ABCModel)(population: Seq[model.Weighted]): Map[model.ParameterSet, Double] = {
			population
			.groupBy(_.parameterSet)
			.map{case (k,v) => (k, v.map(_.weight).sum)}
		}
	}
}