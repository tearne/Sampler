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

/*
 * Empirical implementation backed by a map which counts occurrences of
 * observation values. Ideal for sampling from discrete distributions 
 * where many repeated observations are expected. 
 */
class EmpiricalTable[A](val freqTable: Map[A, Int])(implicit r: Random) extends Empirical[A]{
	def ++(more: GenTraversableOnce[A]) = new EmpiricalTable(
		more.foldLeft(freqTable){case (acc,elem) => 
			acc.updated(elem, acc.getOrElse(elem, 0) + 1)
		}
	)
	
	private lazy val (items, counts) = freqTable.unzip
	private lazy val partition = Partition.fromWeights(counts.map(_.toDouble))
	
	lazy val probabilityTable = items.zip(partition.probabilities).toMap
	
	def toSamplable(implicit r: Random) = Samplable.fromPartition(items.toIndexedSeq, partition)
	
	override def canEqual(other: Any): Boolean = other.isInstanceOf[EmpiricalTable[_]]
}
