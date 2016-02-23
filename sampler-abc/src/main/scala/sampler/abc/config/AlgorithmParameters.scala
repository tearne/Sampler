///*
// * Copyright (c) 2012 Crown Copyright 
// *                    Animal Health and Veterinary Laboratories Agency
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
//import scalaz._
//import Scalaz._
//
//case class AlgorithmParameters(
//	maxParticleRetries: Int,
//	particleChunkSize: Int
//)
//object AlgorithmParameters{
//	val default = AlgorithmParameters(
//		100,
//		500
//	)
//	def fromConfig(c: Config) = AlgorithmParameters(
//		c.getInt("abc.algorithm.particle-retries"),
//		c.getInt("abc.algorithm.particle-chunk-size")
//	)
//	
//	val maxParticleRetriesLens = Lens.lensu[AlgorithmParameters, Int](
//		(o,v) => o.copy(maxParticleRetries = v),
//		_.maxParticleRetries
//	)
//	val particleChunkSizeLens = Lens.lensu[AlgorithmParameters, Int](
//		(o,v) => o.copy(particleChunkSize = v),
//		_.particleChunkSize
//	)
//}