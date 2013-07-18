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

case class Partition(values: IndexedSeq[Probability]) {
	private def isEqualOne(value: Double) = if(value > 1 - 1E-8 && value < 1 + 1E-8) true else false
	assert(isEqualOne(values.map(_.value).sum), s"Expected probabilies to sum to 1, but got ${values.map(_.value).sum}")
	lazy val size = values.size
	lazy val probabilities = values.map(w => Probability(w))
}

object Partition{
	def fromWeights(weights: IndexedSeq[Double]) = {
		val totalWeight = weights.sum

		Partition(weights.map(_ / totalWeight).to[IndexedSeq].map(Probability(_)))
	}
}