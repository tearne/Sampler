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

package sampler.abc.config

import com.typesafe.config.Config
import scalaz.Lens._
import sampler.abc.config.AlgorithmParameters
import sampler.abc.config.JobParameters
import sampler.abc.config.AlgorithmParameters
import sampler.abc.config.ClusterParameters
import sampler.abc.config.JobParameters
import sampler.abc.config.ClusterParameters

case class ABCConfig(
		job: JobParameters,
		algorithm: AlgorithmParameters,
		cluster: ClusterParameters
)

object ABCConfig{
	def fromTypesafeConfig(c: Config, name: String) = {
		val subConfig = c.getConfig(name)
		ABCConfig(
			JobParameters.fromConfig(subConfig),
			AlgorithmParameters.fromConfig(subConfig),
			ClusterParameters.fromConfig(subConfig)
		)
	}
	
	val jobLens = lensu[ABCConfig, JobParameters](
			(o,v) => o.copy(job = v),
			_.job
	)
	
	val algorithmLens = lensu[ABCConfig, AlgorithmParameters](
			(o,v) => o.copy(algorithm = v),
			_.algorithm
	)
	
	val clusterLens = lensu[ABCConfig, ClusterParameters](
			(o,v) => o.copy(cluster = v),
			_.cluster
	)
}