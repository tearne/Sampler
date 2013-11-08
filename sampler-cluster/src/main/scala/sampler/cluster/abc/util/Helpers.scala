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

package sampler.cluster.abc.util

import sampler.abc.ABCModel
import sampler.io.Logging
import sampler.math.Probability
import sampler.math.StatisticsComponent
import sampler.Implicits._

trait Helpers extends Logging{
	self: StatisticsComponent =>
	
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
	
	def calculateNewTolerance(weighedParameters: Seq[ABCModel#Weighted], fallBackTolerance: Double): Double = {
		val medianMeanScore = statistics.quantile(weighedParameters.map{_.meanScore}.toEmpiricalSeq, Probability(0.5))
		if(medianMeanScore == 0) {
			log.warn("Median of mean scores from last generation evaluated to 0, using fallback tolerance: {}", fallBackTolerance)
			fallBackTolerance
		}
		else math.min(medianMeanScore, fallBackTolerance)
	}
	
	def consolidateToWeightsTable(model: ABCModel)(population: Seq[model.Weighted]): Map[model.ParameterSet, Double] = {
		population
			.groupBy(_.parameterSet)
			.map{case (k,v) => (k, v.map(_.weight).sum)}
	}
}