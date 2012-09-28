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

/*
 * Specialisation of Samplable to apply to sets of observed data
 */
trait Empirical[Sample, Domain] extends Samplable[Sample]{ 
	val size: Int
	val probabilityMap: Map[Domain, Probability]
}

/*
 *  Measuring the distance between pairs of Empirical
 */
trait EmpiricalMetricComponent{
	this: StatisticsComponent =>
		
	def mean[T: Fractional](a: Empirical[_,T], b: Empirical[_,T]) = {
		math.abs(statistics.mean(a)-statistics.mean(b))
	}
	
	def max[T](a: Empirical[_,T], b: Empirical[_,T]): Double = {
		val indexes = a.probabilityMap.keySet ++ b.probabilityMap.keySet
		def distAtIndex(i: T) = math.abs(
				a.probabilityMap.get(i).map(_.value).getOrElse(0.0) -
				b.probabilityMap.get(i).map(_.value).getOrElse(0.0)
		)
		indexes.map(distAtIndex(_)).max
	}
}

object EmpiricalMetric extends EmpiricalMetricComponent with StatisticsComponent{
	val statistics = new Statistics()
}
