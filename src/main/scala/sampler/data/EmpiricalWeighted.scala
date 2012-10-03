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
import sampler.math.AliasWrapper

/*
 * Empirical implementation which uses a weight attached to each
 * observation value for sampling.
 */
class EmpiricalWeighted[A](val table: Map[A, Double]) extends Empirical[A]{
	//TODO remove duplication here with EmpiricalWeighted
	//TODO tidy up the Alias stuff, pushing this away
	private lazy val (indexedValues, indexedProbabilities) = probabilities.toIndexedSeq.unzip
	private lazy val alias = new AliasWrapper(indexedProbabilities.map(_.value))
	
	//TODO make alias use random, rather than it's own
	def sample(implicit r: Random) = indexedValues(alias.sample())
	
	lazy val supportSize = table.size
	lazy val probabilities = {
		if(table.find(_._2 <= 0).isDefined) 
			throw new IllegalArgumentException{
				val badValue = table.find(_._2 <= 0).get
				"Weight must be strictly positive, found (%s,%f)".format(badValue._1.toString, badValue._2)
			}
		val totalWeight = table.values.sum
		table.map{case (k,v) => (k,Probability(v / totalWeight))}
	}
	
	// Have not implemented a ++ method since we might attempt to add values
	//which have differnetly scaled weights (normalised, un-normalised, ...)
	//If we really want to do this when we have access to the original
	//un-normalised table which was supplied to the constructor.
	
	def toEmpiricalTable() = new EmpiricalTable(
		table.map{case (k,v) => (k,1)}
	)
	
	def toEmpiricalSeq() = new EmpiricalSeq(table.keys.toIndexedSeq)
	
	override def canEqual[A: Manifest](other: Any): Boolean = other.isInstanceOf[EmpiricalWeighted[_]]
}