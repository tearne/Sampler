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
import sampler.math.Probability
import scala.collection.GenTraversableOnce
import sampler.math.AliasTable
import sampler.math.Partition

/*
 * Empirical implementation which uses a weight attached to each
 * observation value for sampling.
 */
class EmpiricalWeighted[A](val weighted: Map[A, Double]) extends Empirical[A]{
	lazy val (values, weights) = weighted.unzip
	lazy val partition = Partition.fromWeights(weights.toIndexedSeq)
	
	lazy val probabilityTable = values.zip(partition.probabilities).toMap
	
	def toSamplable(implicit r: Random) = Samplable.fromPartition(values, partition)
	
	// Have not implemented a ++ method since it might be used to add values
	//which have differently scaled weights (normalised, un-normalised, ...)
	//If we really want to do this we have access to the original
	//un-normalised weights table as supplied to the constructor, but the 
	//user still needs to be careful that the weights are on equal footing.
	
	override def canEqual(other: Any): Boolean = other.isInstanceOf[EmpiricalWeighted[_]]
}
