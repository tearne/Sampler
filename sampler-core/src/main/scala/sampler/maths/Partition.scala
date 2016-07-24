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

package sampler.maths

import sampler._

/** Contains a set of probabilities which sum to one
 * 
 * Class takes an argument of a collection of probabilities in the form of an IndexedSeq.
 * The total of all probabilities in the collection should sum to one.
 * The primary use of a Partition is to be the argument provided to the AliasTable.
 * 
 * @constructor create a new Partition from a sequence of probabilities
 * @param probabilities the probabilities used to form the Partition
 */
case class Partition(val probabilities: IndexedSeq[Double]) {
  
	private def isCloseToOne(value: Double) = if(value > 1 - 1E-8 && value < 1 + 1E-8) true else false
	
	assert(isCloseToOne(probabilities.sum), s"Expected probabilies to sum to 1, but got ${probabilities.sum}")
	assert(probabilities.areProbabilities)

	/** the number of probabilties in the Partition */
	lazy val size = probabilities.size
}

/** Factory for Partition instances */
object Partition{
  
  /** Creates Partition from collection of values 
   *  
   *  @param weights a sequence of values which are normalised to form a new Partition
   *  */
	def fromWeights(weights: IndexedSeq[Double]) = {
		val totalWeight = weights.sum
		Partition(weights.map(_ / totalWeight).to[IndexedSeq])
	}
}