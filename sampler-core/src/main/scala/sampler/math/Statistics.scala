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

package sampler.math

import sampler.data.Empirical

trait StatisticsComponentImpl{
	val statistics = new Statistics{}
}


trait StatisticsComponent{
	val statistics: Statistics
}

trait Statistics{
	
  /** Returns the proportion (0-1 range) of items in Empirical which are greater than or equal to supplied value
   *  
   *  @param itemInclusive The value of interest, return value is inclusive of this value
   *  @return a new Probability giving the right tail
   *  */
	def rightTail[A](e: Empirical[A], itemInclusive: A)(implicit o: Ordering[A]): Double = {
		import e._
		val value = probabilityTable.keys.toList.sorted(o).dropWhile(i => o.lt(i,itemInclusive)).foldLeft(0.0){
			case (acc, i) => acc + probabilityTable(i)
		}
		value
	}
	
	/** Takes a sequence of probabilities and returns the associated quantile values from an Empirical
	 *  
	 *  @param e
	 *  @param prob The required quantile values
	 *  @return A sequence of the quantile values
	 */
	def quantiles[A](e: Empirical[A], probs: Seq[Double])(implicit f: Fractional[A]): Seq[A] = {
		import e._

		probs.foreach{p => RangeCheck.probability(p)}
		
		val probabilities = e.probabilityTable
		
		val ordered = probabilities.keys.toIndexedSeq.sorted
		
		assert(ordered.length > 0, "Cannot work out quantiles of an Empirical object with zero values")

		val cumulativeProbability = ordered.map(value => probabilities(value)).scanLeft(0.0)(_ + _).tail
		
		// Tolerance required to prevent incorrect results due to rounding errors
		// Resulting quantiles are consistent with R type 1
		val index = probs.map(prob => cumulativeProbability.zipWithIndex.find(_._1 >= (prob - 1e-6)).get._2)	
		
		index.map(ordered(_))
	}
	
	/** Convenience method for calculating a single quantile value from an Empirical.  To avoid overheads 
	 *  when calculating multiple times use [[sampler.math.Statistics.quantiles]]
	 *  
	 *  @param prob The required quantile value
	 *  @return The quantile value
	 */
	def quantile[A](e: Empirical[A], prob: Double)(implicit f: Fractional[A]): A = quantiles(e, Seq(prob))(f).head
	
	/** Returns the mean value of an Empirical
	 */
	def mean[A](e: Empirical[A])(implicit num: Fractional[A]) = {
		import num._
		e.probabilityTable.foldLeft(0.0){case (acc, (v,p)) => {
			acc + v.toDouble * p
		}}
	}
	
	/** Returns the difference between the mean of two Empiricals
	 *  
	 *  A metric for calculating the difference between two Empiricals, using the mean value of the 
	 *  two Empiricals
	 *  
	 *  @return The difference between means
	 */
	def meanDistance[A: Fractional](a: Empirical[A], b: Empirical[A]) = {
		math.abs(mean(a)-mean(b))
	}
  
	/** Returns to maximum difference between two Empiricals
	 *  
	 *  An alternative metric for calculating the distance between two Empiricals. The distance is defined
	 *  as the greatest distance between the probabilities of each individual value in two distributions.
	 *  
	 *  E.g. In Empirical(1, 2) the probabilities are 1 -> 0.5, 2 -> 0.5. In Empirical(1,2,2,2,3) 
	 *  the probabilities are 1 -> 0.2, 2 -> 0.6, 3 -> 0.2. The differences are therefore 1 -> 0.3, 
	 *  2 -> 0.1 and 3 -> 0.2 and thus the max distance is 0.3 
	 *  
	 *  @return The max difference
	 */
	def maxDistance[A](a: Empirical[A], b: Empirical[A]): Double = {
		val indexes = a.probabilityTable.keySet ++ b.probabilityTable.keySet
		def distAtIndex(i: A) = math.abs(
			a.probabilityTable.get(i).getOrElse(0.0) -
			b.probabilityTable.get(i).getOrElse(0.0)
		)
		indexes.map(distAtIndex(_)).max
	}
}

object Statistics extends Statistics with Serializable