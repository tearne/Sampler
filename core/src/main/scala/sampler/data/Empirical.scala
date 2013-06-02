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

/*
 * Collections of observations, forming empirical distributions.
 * No variance in [A] since the probabilities Map[A, Probability] 
 * can't support it 
 */
trait Empirical[A] extends Serializable{
	// Relative frequency associated with each observation value 
	val probabilityTable: Map[A, Probability]

	// Number of _unique_ observations (not overall number of observations)
	def supportSize: Int = probabilityTable.size

	//Implementation different depending on backing collection
	def toSamplable(implicit r: Random): Samplable[A]
	
	def canEqual(other: Any): Boolean = other.isInstanceOf[Empirical[_]]
	override def equals(other: Any) = other match {
		//Implement equality in terms of the probabilities of drawing values
		case that: Empirical[_] => 
			(that canEqual this) && (that.probabilityTable == probabilityTable)
		case _ => false
	}
	
	override def hashCode() = probabilityTable.hashCode
}

/*
 * TODO The user has to remember to import the contents of this object for the
 * pimps to work.  
 * import sampler.data.Empirical._
 * 
 * Is there a better way to achieve the same thing? 
 */
object Empirical{
	implicit class RichIndexedSeq[A](genSeq: GenSeq[A])(implicit r: Random) {
		val indSeq = genSeq.toIndexedSeq
		def toEmpiricalSeq = new EmpiricalSeq[A](indSeq)
		def toEmpiricalTable = new EmpiricalTable[A](
			indSeq.groupBy(identity).map{case (k,v) => k -> v.size}
		)
	}
	
	implicit class RichMapInt[A](table: GenMap[A,Int])(implicit r: Random) {
		def toEmpiricalTable = {
			if(table.values.find(_ <= 0).isDefined) throw new UnsupportedOperationException("Cannot convert to EmpiricalTable, non-positive counts found")
			else new EmpiricalTable[A](table.seq.toMap)
		}
	}
	
	implicit class RichMapDouble[A](table: GenMap[A,Double])(implicit r: Random) {
		def toEmpiricalWeighted = {
			if(table.values.find(_ <= 0).isDefined) throw new UnsupportedOperationException("Cannot convert to EmpiricalWeighted, non-positive weights found")
			else new EmpiricalWeighted[A](table.seq.toMap)
		}
	}
	
	implicit class RichMapProbability[A](table: Map[A, Probability])(implicit r: Random) {
		def toEmpiricalWeighted = new EmpiricalWeighted[A](table.map{case (k,v) => (k,v.value)})
	}
}

/*
 *  Measuring the distance between pairs of Empirical
 */
//TODO test

trait EmpiricalMetricComponent extends StatisticsComponent {
		
  def absoluteMean[A: Fractional](a: Empirical[A], b: Empirical[A]) = {
    math.abs(mean(a)-mean(b))
  }
  
  def max[A](a: Empirical[A], b: Empirical[A]): Double = {
    val indexes = a.probabilityTable.keySet ++ b.probabilityTable.keySet
    def distAtIndex(i: A) = math.abs(
        a.probabilityTable.get(i).map(_.value).getOrElse(0.0) -
        b.probabilityTable.get(i).map(_.value).getOrElse(0.0)
    )
    indexes.map(distAtIndex(_)).max
  }
}

