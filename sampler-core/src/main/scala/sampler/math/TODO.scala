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

import sampler._

object TODO {
	/** Returns the difference between the mean of two Empiricals
	 *  
	 *  A metric for calculating the difference between two Empiricals, using the mean value of the 
	 *  two Empiricals
	 *  
	 *  @return The difference between means
	 */
//	def meanDistance[A: Fractional](a: Empirical[A], b: Empirical[A]) = {
//		scala.math.abs(mean(a)-mean(b))
//	}
  
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
//	def maxDistance[A](a: Empirical[A], b: Empirical[A]): Double = {
//		val indexes = a.probabilityTable.keySet ++ b.probabilityTable.keySet
//		def distAtIndex(i: A) = scala.math.abs(
//			a.probabilityTable.get(i).getOrElse(0.0) -
//			b.probabilityTable.get(i).getOrElse(0.0)
//		)
//		indexes.map(distAtIndex(_)).max
//	}
}