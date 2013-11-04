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

package sampler.cluster.abc.slave

import sampler.abc.ABCModel
import sampler.math.Random
import sampler.run.WrappedAborter
import scala.util.Try
import sampler.cluster.abc.ABCJob
import java.util.concurrent.atomic.AtomicBoolean
import sampler.abc.builder.ParticleBuilderComponent

trait ParticleGenerator {
	self: ParticleBuilderComponent =>
		
	val model: ABCModel
	val random: Random
	
	val aborted: AtomicBoolean = new AtomicBoolean(false)

	def abort() { aborted.set(true) }
	def isAborted = aborted.get
	def reset() { aborted.set(false) }
	
	def apply = PartialFunction[Any, Try[ABCModel#Population]]{
		case job: ABCJob[_] => 
			//import job._
			Try{
				particleBuilder(model)(
					job.population.asInstanceOf[model.Population], 
					job.abcParams.particleChunking,
					job.currentTolerance,
					new WrappedAborter(aborted),
					job.abcParams,
					random
				)
			}
	}
}

object ParticleGenerator{
	def apply(model0: ABCModel, random0: Random) = new ParticleGenerator with ParticleBuilderComponent {
		val model = model0
		val random = random0
		val particleBuilder = new ParticleBuilder{}
	}
}

