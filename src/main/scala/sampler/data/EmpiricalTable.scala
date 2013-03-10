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

/*
 * Empirical implementation backed by a map which counts occurrences of
 * observation values. Ideal for sampling from discrete distributions 
 * where many repeated observations are expected. 
 */
class EmpiricalTable[A](val counts: Map[A, Int])(implicit val o: Ordering[A] = null, val f: Fractional[A] = null) extends Empirical[A]{
	private lazy val (indexedValues, indexedProbabilities) = probabilities.toIndexedSeq.unzip

	private lazy val aliasTable = new AliasTable(indexedProbabilities)
	
  val r = new Random()

	def sample() = indexedValues(aliasTable.next(r))
	
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
	
	/*
	 * Throw away weights, producing a seq with one observation of every 
	 * potential value, ie uniform sampling.
	 * 
	 * TODO Q: Are we sure we want to just take keys? 
	 *         What about duplicating observations as per the counts?
	 *      A: Currently done this way for consistency with EmpiricalWeighted
	 */
	def toEmpiricalSeq() = new EmpiricalSeq(counts.keys.toIndexedSeq)
	
	override def canEqual(other: Any): Boolean = other.isInstanceOf[EmpiricalTable[_]]
}
