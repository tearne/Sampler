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

case class ABCParameters(
		reps: Int, 
		numParticles: Int, 
		startTolerance: Double = Double.MaxValue, 
		refinements: Int, 
		particleRetries: Int = 100, 
		particleChunking: Int = 100
)

case class Particle[A](value: A, weight: Double, meanFit: Double)

trait Prior[A] extends Distribution[A]{
	def density(value: A): Double
}

trait ABCModel{
	type Parameters <: ParametersBase
	protected trait ParametersBase {
		def perturb(): Parameters
		def perturbDensity(that: Parameters): Double		
	}
	
	type Observed
	val observed: Observed
	
	type Simulated <: SimulatedBase
	protected trait SimulatedBase {
		def distanceToObserved: Double
	}
	
	val prior: Prior[Parameters]

	def modelDistribution(p: Parameters): Distribution[Simulated]

	type Population = Seq[Particle[Parameters]]
}
