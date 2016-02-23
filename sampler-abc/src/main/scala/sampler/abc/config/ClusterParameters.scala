///*
// * Copyright (c) 2012-13 Crown Copyright 
// *                       Animal Health and Veterinary Laboratories Agency
// *
// * Licensed under the Apache License, Version 2.0 (the "License");
// * you may not use this file except in compliance with the License.
// * You may obtain a copy of the License at
// *
// *     http://www.apache.org/licenses/LICENSE-2.0
// *
// * Unless required by applicable law or agreed to in writing, software
// * distributed under the License is distributed on an "AS IS" BASIS,
// * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// * See the License for the specific language governing permissions and
// * limitations under the License.
// */
//
//package sampler.abc.config
//
//import com.typesafe.config.Config
//import scalaz.Lens._
//import java.util.concurrent.TimeUnit
//
//case class ClusterParameters(
//		terminateAtTargetGenerations: Boolean,
//		particleMemoryGenerations: Int,
//		futuresTimeoutMS: Long,
//		mixPayloadSize: Int,
//		mixRateMS: Long,
//		mixResponseTimeoutMS: Long,
//		sizeReportingMS: Long
//)
//
//object ClusterParameters{
//	def fromConfig(c: Config) = {
//		val subConf = c.getConfig("abc.cluster")
//		import subConf._
//		ClusterParameters(
//			getBoolean("terminate-at-target-generation"),
//			getInt("particle-memory-generations"),
//			getDuration("futures-timeout", TimeUnit.MILLISECONDS),
//			getInt("mixing.num-particles"),
//			getDuration("mixing.rate", TimeUnit.MILLISECONDS),
//			getDuration("mixing.response-threshold", TimeUnit.MILLISECONDS),
//			getDuration("size-reporting", TimeUnit.MILLISECONDS) //TODO untested
//		)
//	}
//	
//	val mixRateMSLens = lensu[ClusterParameters, Long](
//			(o,v) => o.copy(mixRateMS = v),
//			_.mixRateMS
//	)
//}