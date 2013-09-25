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

package sampler.abc.population

import sampler.abc.ABCModel
import sampler.data.Empirical
import sampler.math.Random
import sampler.cluster.actor.worker.Executor
import sampler.run.WrappedAborter

class ActorPopulationExecutor(model: ABCModel, random: Random) extends Executor{
	def run = PartialFunction[Any, Any]{
		case ABCJob(pop, quantity, tol, meta) => 
			Population(model)(
					pop.asInstanceOf[model.Population], 
					quantity, 
					tol, 
					new WrappedAborter(aborted),
					meta,
					random
			)
	}
}