/*
 * Copyright (c) 2012 Crown Copyright 
 *                    Animal Health and Veterinary Laboratories Agency
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

package sampler.abc

import sampler.math.{ Random, StatisticsComponent }
import sampler.data.Distribution

trait Prior[A] extends Distribution[A]{
	def density(value: A): Double
}
case class Scored[+A](value: A, runScores: Seq[Double])
case class Weighted[+A](scored: Scored[A], weight: Double){
	def value = scored.value
	def runScores = scored.runScores
	def meanScore = runScores.sum.toDouble / runScores.size
}

trait ABCModel{
	type ParameterSet <: ParameterSetBase
	protected trait ParameterSetBase {
		def perturb(): ParameterSet
		def perturbDensity(that: ParameterSet): Double		
	}
	
	type Observed
	val observed: Observed
	
	type Simulated <: SimulatedBase
	protected trait SimulatedBase {
		def distanceToObserved: Double
	}
	
	val prior: Prior[ParameterSet]

	def modelDistribution(p: ParameterSet): Distribution[Simulated]
	
//	case class Scored(parameterSet: ParameterSet, runScores: Seq[Double])
//	case class Weighted(scored: Scored, weight: Double){
//		def parameterSet = scored.parameterSet
//		def runScores = scored.runScores
//		def meanScore = runScores.sum.toDouble / runScores.size
//	}
}
