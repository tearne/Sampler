/*
 * Copyright (c) 2012-14 Crown Copyright 
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

package sampler.oldempirical

import sampler.math._
import scala.collection.GenSeq
import scala.collection.immutable.Map
import scala.collection.GenMap
import sampler.distribution.Distribution

/** Empirical distribution of a collection of observations
 *  
 *  Collections of observations, forming empirical di
import sampler.empirical.EmpiricalSeqstributions.
 *  No variance in [A] since the probabilities Map[A, Probability] 
 *  can't support it 
 */
trait Empirical[A] extends Serializable{
  
	/** Relative frequency associated with each observation value */
	val probabilityTable: Map[A, Double]

	/** The number of observations
	 *  
	 *  @return Number of observations, including repetitions */
	val size: Int
	
	/** The size of the distribution's support
	 *  
	 *  @return Number of unique observations (not overall number of observations) */
	def supportSize: Int = probabilityTable.size

	/** Creates a new [[sampler.data.Distribution]] object created from this distribution.
	 *  The implementation differs depending on backing collection
	 *  
	 *  @return [[sampler.data.Distribution]] object
	 */
	def toDistribution(implicit r: Random): Distribution[A]
	
	/** Tests whether this instance is of the same type as another, and thus has the potential to be equal
	 *  
	 *  @return true if this and that are of the same type
	 */
	def canEqual(that: Any): Boolean = that.isInstanceOf[Empirical[_]]
	
	/** Tests whether this instance is equal to another
	 *  
	 *  @return true if two this and that are equal */
	override def equals(that: Any) = that match {
		//Implement equality in terms of the probabilities of drawing values
		case that: Empirical[_] => 
			(that canEqual this) && (that.probabilityTable == probabilityTable)
		case _ => false
	}
	
	override def hashCode() = probabilityTable.hashCode
}