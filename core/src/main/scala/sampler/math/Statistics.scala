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

package sampler.math

import sampler.data.Empirical

/*
 * Mix in the StatisticsComponent to enable easy calculation of stats on Empirical
 */
trait StatisticsComponent{
	def rightTail[A](e: Empirical[A], itemInclusive: A)(implicit o: Ordering[A]): Probability = {
    import e._
		val value = probabilityTable.keys.toList.sorted(o).dropWhile(i => o.lt(i,itemInclusive)).foldLeft(0.0){
			case (acc, i) => acc + probabilityTable(i).value
		}
		Probability(value)
	}
	
	//TODO take seq of probability and give seq of results
	def quantile[A](e: Empirical[A], prob: Probability)(implicit f: Fractional[A]): A = {
		//TODO exception/assertion that there are more than 0 / 1 items to enable
		// this to run without IndexOutOfBoundsException
		import e._
		import f._
		val (lower, upper) = {
			val raw = prob.value * supportSize - 1
			val idx = scala.math.ceil(raw).toInt
			if(idx <= 0) (0,0)
			else if(raw != math.floor(raw)) (idx, idx)
			else if(idx == supportSize - 1) (idx, idx)
			else (idx, idx + 1)
		}
		
		val two = one + one
		val ordered = probabilityTable.keys.toIndexedSeq.sorted(f)
		
		println(ordered)
		
		(ordered(lower) + ordered(upper)) / two 
	}
	
	def mean[A](e: Empirical[A])(implicit num: Fractional[A]) = {
		import num._
		e.probabilityTable.foldLeft(0.0){case (acc, (v,p)) => {
			acc + v.toDouble * p.value
		}}
	}
}

object StatisticsComponent extends StatisticsComponent with Serializable

