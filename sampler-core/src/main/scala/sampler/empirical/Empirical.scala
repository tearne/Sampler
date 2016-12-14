package sampler.empirical

import sampler._

trait Empirical[A] {
  val observationCount: Int
  val probabilityTable: Map[A, Double]
  
  /** The proportion (0-1 range) of items in the Empirical which are greater than or equal to supplied value (inclusive)
   *  
   *  @param itemInclusive The value of interest, return value is inclusive of this value
   *  @return Probability representing the proportion of items in the right tail
   **/
	def rightTail(itemInclusive: A)(implicit o: Ordering[A]): Double = {
    val result = probabilityTable.keys
		  .toList
		  .sorted
		  .dropWhile( i => o.lt(i,itemInclusive) )
		  .foldLeft(0.0){ case (acc, i) => acc + probabilityTable(i) }
		result
	}
  
	/** Takes a sequence of probabilities and returns the associated percentile values from an Empirical
	 *  
	 *  @param e
	 *  @param prob The required quantile values
	 *  @return A sequence of the quantile values
	 */
	def percentile(probs: Seq[Double])(implicit f: Fractional[A]): Seq[A] = {
		assert(probs.areProbabilities)
		
		val ordered = probabilityTable.keys.toIndexedSeq.sorted
		
		assert(ordered.length > 0, "Cannot work out quantiles of an Empirical object with zero values")

		val cumulativeProbability = ordered.map(value => probabilityTable(value)).scanLeft(0.0)(_ + _).tail
		
		// Tolerance required to prevent incorrect results due to rounding errors
		// Resulting quantiles are consistent with R type 1
		val index = probs.map(prob => cumulativeProbability.zipWithIndex.find(_._1 >= (prob - 1e-6)).get._2)	
		
		index.map(ordered(_))
	}
	
	/** Convenience method for calculating a single quantile value.  To avoid overheads 
	 *  when calculating multiple times use [[sampler.math.Statistics.quantiles]]
	 *  
	 *  @param prob The required quantile value
	 *  @return The quantile value
	 */
	def percentile(prob: Double)(implicit f: Fractional[A]): A = percentile(Seq(prob))(f).head
	
	/** Returns the mean value of an Empirical
	 */
	def mean(implicit num: Fractional[A]): Double = {
		import num._
		probabilityTable.foldLeft(0.0){case (acc, (v,p)) => {
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
	def meanDistanceTo(other: Empirical[A])(implicit f: Fractional[A]) = {
		math.abs(mean - other.mean)
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
	def maxDistanceTo(other: Empirical[A]): Double = {
		val indexes = probabilityTable.keySet ++ other.probabilityTable.keySet
		def distAtIndex(i: A) = math.abs(
			probabilityTable.get(i).getOrElse(0.0) -
			other.probabilityTable.get(i).getOrElse(0.0)
		)
		indexes.map(distAtIndex(_)).max
	}
}