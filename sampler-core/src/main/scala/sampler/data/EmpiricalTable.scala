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

package sampler.data

import sampler.math.Random
import scala.collection.GenTraversableOnce
import sampler.math.Probability
import sampler.math.AliasTable
import sampler.math.Partition

/** Empirical implementation which is backed by a Map which counts occurences of observations.
 * 
 * Ideal for sampling from discrete distributions where many repeated observations are expected
 */
class EmpiricalTable[A](val freqTable: Map[A, Int]) extends Empirical[A]{
	assert(freqTable.size > 0, "Cannot create empirical from collection of size zero")
	
	lazy val size = freqTable.values.sum
	
	private lazy val (items, counts) = freqTable.unzip
	private lazy val partition = Partition.fromWeights(counts.map(_.toDouble).toIndexedSeq)
	
	/** A map from each observation to the probability of seeing that value */
	lazy val probabilityTable = items.zip(partition.probabilities).toMap
	
	/**Returns a new Empirical containing a combined oberseration map of the observations
	 * in this instance and those in the more instance
	 * 
	 * {{{
	 * val empTable = new EmpiricalTable(IndexedSeq(1,2,3)
	 * val more = IndexedSeq(3)
	 * 
	 * empTable ++ more
	 * }}}
	 * 
	 * @param more the observations to append
	 * @return new Empirical containing combined observations
	 */
	def ++(more: GenTraversableOnce[A]) = new EmpiricalTable(
		more.foldLeft(freqTable){case (acc,elem) => 
			acc.updated(elem, acc.getOrElse(elem, 0) + 1)
		}
	)
	
	/** Creates a new [[sampler.data.Distribution]] from the Empirical Table
     *  
     *  @return [[sampler.data.Distribution]] object */
	def toDistribution(implicit r: Random): Distribution[A] = Distribution.fromPartition(items.toIndexedSeq, partition)
	
	override def canEqual(other: Any): Boolean = other.isInstanceOf[EmpiricalTable[_]]
}
