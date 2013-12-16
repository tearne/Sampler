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

package sampler.abc.parameters

import scalaz._
import Scalaz._
import com.typesafe.config.Config

case class ABCParameters(job: JobParameters, algorithm: AlgorithmParameters)

object ABCParameters{
	//TODO test
	def fromConfig(c: Config) = ABCParameters(
		JobParameters.fromConfig(c),
		AlgorithmParameters.fromConfig(c)
	)
	
	val algorithmLens = Lens.lensu[ABCParameters, AlgorithmParameters](
		(o,v) => o.copy(algorithm = v),
		_.algorithm
	)
	val jobLens = Lens.lensu[ABCParameters, JobParameters](
		(o,v) => o.copy(job = v),
		_.job
	)
	
}