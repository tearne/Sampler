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

import sampler.math._
import scala.annotation.tailrec
import scala.collection.parallel.ParSeq
import scala.collection.GenSeq
import scala.collection.Seq
import scala.collection.IndexedSeq
import scala.collection.TraversableOnce
import scala.math.Ordering
import scala.math.Fractional
import sampler.math.StatisticsComponent
import scala.collection.immutable.Map
import scala.collection.GenMap

/** Empirical distribution of a collection of observations
 *  
 *  Collections of observations, forming empirical distributions.
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

/** Factory for creating Empirical instances
 *  
 *  Allows Empiricals to be created from standard collections
 */
trait ToEmpirical {
	implicit class RichIndexedSeq[A](genSeq: GenSeq[A]) {
		val indSeq = genSeq.toIndexedSeq
		def toEmpiricalSeq = new EmpiricalSeq[A](indSeq)
		def toEmpiricalTable = new EmpiricalTable[A](
			indSeq.groupBy(identity).map{case (k,v) => k -> v.size}
		)
	}
	
	implicit class RichMapInt[A](table: GenMap[A,Int]) {
		def toEmpiricalTable = {
			if(table.values.find(_ <= 0).isDefined) throw new UnsupportedOperationException("Cannot convert to EmpiricalTable, non-positive counts found")
			else new EmpiricalTable[A](table.seq.toMap)
		}
	}
}

