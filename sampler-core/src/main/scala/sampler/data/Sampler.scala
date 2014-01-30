/*
 * Copyright (c) 2012-13 Crown Copyright 
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

package sampler.data

import scala.collection.GenSeq
import scala.collection.parallel.ParSeq

/** For producing samples from a distribution until a condition is met */

trait Sampler{
  /** Collects samples from distribution until condition returns true
   *  
   *  @param distribution Distribution object from which samples can be drawn
   *  @param condition Function to decide when to stop sampling
   *  @return Collection of sampled elements
   */
	def apply[T](distribution: Distribution[T])(condition: GenSeq[T] => Boolean): GenSeq[T]
}


//TODO add options making it easier to test the condition in the client code
/*
 * e.g. currently it's a pain:
 * 
 * 			new ParallelSampler(chunkSize)(samplable)(seq => {
 *				statistics.maxDistance(seq.take(seq.size - chunkSize).toEmpiricalSeq, seq.toEmpiricalSeq) < tolerance ||
 *					seq.size == 1e8
 *			})
 * 
 */


//TODO say in the docs that this just uses Dist.until
/** Serializable implementation of [[sampler.data.Sampler]], uses until method of [[sampler.data.Distribution]] 
 *  to sample from the distribution
 */
object SerialSampler extends Sampler with Serializable{
	def apply[T](distribution: Distribution[T])(condition: GenSeq[T] => Boolean) = 
		distribution.until(condition).sample()
}

//TODO docs
//TODO give warning about the perils of non thread safe dists.  With commons math normal dist example
//     See http://stackoverflow.com/questions/20969292/thread-safety-warnings
class ParallelSampler(chunkSize: Int) extends Sampler{
	def apply[T](distribution: Distribution[T])(condition: GenSeq[T] => Boolean) = {
		def takeMore(previous: ParSeq[T]): ParSeq[T] = {
			if(condition(previous)) previous
			else takeMore(
					previous ++ (1 to chunkSize).par.map(i => distribution.sample)
			)
		}
		val kickstart = (1 to chunkSize).par.map(i => distribution.sample)
		takeMore(kickstart)
	}
}
