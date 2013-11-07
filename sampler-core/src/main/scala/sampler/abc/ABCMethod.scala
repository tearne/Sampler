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
import sampler.math.StatisticsComponent
import sampler.io.Logging
import sampler.abc.generation._
import sampler.abc.builder.PopulationBuilder
import sampler.math.Statistics

trait ABCMethod extends InitialiseComponent
		with IterateComponent
		with StatisticsComponent
		with Logging{
	
	def apply[M <: ABCModel](
			model: M,
			abcParams: ABCParameters, 
			populationBuilder: PopulationBuilder,
			random: Random
	): Option[Seq[model.ParameterSet]] = {
		val ePop0 = initialise(model, abcParams)
		val ePopFinal = iterate(ePop0, abcParams, populationBuilder, random)
		ePopFinal.map(
			//TODO MS: Don't like this cast
			_.asInstanceOf[EncapsulatedPopulation[model.type]]
			.population.map{
				_.parameterSet
			}
		)
	}
}

object ABCMethod extends ABCMethod {
	val initialise = new Initialise{}
	val iterate = new Iterate{}
	val statistics = new Statistics{}
}