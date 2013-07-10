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

package sampler.math

case class Partition(values: IndexedSeq[Double]) {
	private def isEqualOne(value: Double) = if(value > 1 - 1E-8 && value < 1 + 1E-8) true else false
	assert(isEqualOne(values.sum), s"Expected probabilies to sum to 1, but got ${values.sum}")
	lazy val size = values.size
	lazy val probabilities = values.map(w => Probability(w))
}
object Partition{
	def fromWeights(weights: Iterable[Double]) = {
		//TODO use pattern matching here
		//TODO allow zeros in the weights, provided it's not going to cause massive issues in the Alias table sampling
		//TODO this exception doesn't seem to supply a message when thrown
		if(weights.find(_ <= 0).isDefined) throw new IllegalArgumentException({
			val badValue = weights.find(_ <= 0).get
			s"Weight must be strictly positive, found $badValue"
		})
		
		val totalWeight = weights.sum

		Partition(weights.map(_ / totalWeight).to[IndexedSeq])
	}
}