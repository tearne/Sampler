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

import sampler.math.{ Random, RandomFactory }
import sampler.math.Probability
import scala.collection.GenTraversableOnce
import sampler.math.AliasTable

/*
 * Empirical implementation which uses a weight attached to each
 * observation value for sampling.
 */
class EmpiricalWeighted[A](val weights: Map[A, Double])(implicit rs: RandomFactory) extends Empirical[A]{
	//TODO Tidy up the Alias stuff, pushing this stuff away
	private lazy val (indexedValues, indexedProbabilities) = probabilities.toIndexedSeq.unzip
	
	private lazy val aliasTable = new AliasTable(indexedProbabilities)
	
	val r = rs.newRandom

	def sample() = indexedValues(aliasTable.next(r))
	
	lazy val supportSize = weights.size
	lazy val probabilities = {
		if(weights.find(_._2 <= 0).isDefined) 
			throw new IllegalArgumentException{
				val badValue = weights.find(_._2 <= 0).get
				"Weight must be strictly positive, found (%s,%f)".format(badValue._1.toString, badValue._2)
			}
		val totalWeight = weights.values.sum
		weights.map{case (k,v) => (k, Probability(v / totalWeight))}
	}
	
	// Have not implemented a ++ method since it might be used to add values
	//which have differently scaled weights (normalised, un-normalised, ...)
	//If we really want to do this we have access to the original
	//un-normalised weights table as supplied to the constructor, but the 
	//user still needs to be careful that the weights are on equal footing.
	
	/*
	 * Throw away weights, producing a table with one observation of every 
	 * potential value, ie uniform sampling.
	 */
	def toEmpiricalTable() = new EmpiricalTable(
		weights.map{case (k,v) => (k,1)}
	)
	
	/*
	 * Throw away weights, producing a seq with one observation of every 
	 * potential value, ie uniform sampling.
	 */
	def toEmpiricalSeq() = new EmpiricalSeq(weights.keys.toIndexedSeq)
	
	override def canEqual(other: Any): Boolean = other.isInstanceOf[EmpiricalWeighted[_]]
}
