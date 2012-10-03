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
import sampler.math.AliasWrapper

/*
 * Empirical implementation backed by a map which counts occurrances.  
 * Ideal for sampling from discrete distributions where many repeated 
 * observations are expected. 
 */
class EmpiricalTable[A](val counts: Map[A, Int]) extends Empirical[A]{
	//TODO remove duplication here with EmpiricalWeighted
	//TODO tidy up the Alias stuff, pushing this away
	private lazy val (indexedValues, indexedProbabilities) = probabilities.toIndexedSeq.unzip
	private lazy val alias = new AliasWrapper(indexedProbabilities.map(_.value))
	
	//TODO make alias use random, rather than it's own
	def sample(implicit r: Random) = indexedValues(alias.sample())
	
	lazy val supportSize = counts.size
	lazy val probabilities = {
		val sizeAsDouble = counts.values.sum.asInstanceOf[Double]
		counts.map{case (k,v) => (k,Probability(v/sizeAsDouble))}
	}
	
	def ++(more: GenTraversableOnce[A]) = new EmpiricalTable(
		more.foldLeft(counts){case (acc,elem) => 
			acc.updated(elem, acc.getOrElse(elem, 0) + 1)
		}
	)
	
	
}