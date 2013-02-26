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

import sampler.math.Random

trait EncapsulatedPopulation[R <: Random] extends Serializable{ self =>
	type M <: ABCModel[R]
	val model: M
	val population: Seq[Particle[model.Parameters]]
	
	def update(population0: Seq[Particle[model.Parameters]]) = EncapsulatedPopulation[R](model)(population0)
}

object EncapsulatedPopulation{
	def apply[R <: Random](model0: ABCModel[R])(population0: Seq[Particle[model0.Parameters]]) = new EncapsulatedPopulation[R] with Serializable{
		type M = model0.type
		val model: M = model0
		val population = population0
	}
}