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
import sampler.data.Samplable

trait ABCModel{
	val statistics: StatisticsComponent

	type Parameters <: ParametersBase
	protected trait ParametersBase {
		def perturb(): Parameters
		def perturbDensity(that: Parameters): Double		
	}
	
	type Observations <: ObservationsBase
	protected trait ObservationsBase
	
	type Output <: OutputBase
	protected trait OutputBase {
		def distanceTo(observed: Observations): Double
	}
	
	val prior: Prior[Parameters]
	val observations: Observations

	def samplableModel(p: Parameters, obs: Observations): Samplable[Output]

	type Population = Seq[Particle[Parameters]]
}
