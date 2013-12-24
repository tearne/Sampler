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

package sampler.cluster.abc.parameters

import com.typesafe.config.Config

case class ClusterParameters(
		terminateAtTargetGenerations: Boolean,
		particleMemoryGenerations: Int,
		futuresTimeoutMS: Long,
		mixPayloadSize: Int,
		mixRateMS: Long,
		mixResponseTimeoutMS: Long
)

object ClusterParameters{
	def fromConfig(c: Config) = {
		val subC = c.getConfig("sampler.abc.cluster")
		ClusterParameters(
			subC.getBoolean("terminate-at-target-generation"),
			subC.getInt("particle-memory-generations"),
			subC.getMilliseconds("futures-timeout"),
			subC.getInt("mixing.num-particles"),
			subC.getMilliseconds("mixing.rate"),
			subC.getMilliseconds("mixing.response-threshold")
		)
	}
}