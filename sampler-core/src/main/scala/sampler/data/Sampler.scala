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

trait Sampler{
	def apply[T](distribution: Distribution[T])(condition: GenSeq[T] => Boolean): GenSeq[T]
}

object SerialSampler extends Sampler with Serializable{
	def apply[T](distribution: Distribution[T])(condition: GenSeq[T] => Boolean) = 
		distribution.until(condition).sample()
}

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
