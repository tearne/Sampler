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

package sampler.data;

import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import sampler.math.Random
import sampler.math.Probability
import scala.math.Fractional
import org.specs2.matcher.DataTables

@RunWith(classOf[JUnitRunner])
class FrequencyTableSpec extends Specification with Mockito with DataTables{
	val once = 1
	val twice = 2
	val thrice = 3
	
	//	--d1--		--d2--		---d3---
	//					6		    3
	//				  5,6		  2,3
	//	4,5,6		4,5,6		1,2,3,4
	val d1 = FrequencyTable[Int](IndexedSeq(4, 5, 6))
	val d2 = FrequencyTable[Int](IndexedSeq(4, 5,5, 6,6,6))
	val d3 = FrequencyTable[Int](IndexedSeq(1, 2,2, 3,3,3, 4))
	
	"FrequencyTable" should {
		"know number of observations" in {
			(d1.size mustEqual 3) and
			(d2.size mustEqual 6) and
			(d3.size mustEqual 7)
		}
		
		"calculate probability as relative frequency" in {
			val map1 = d1.probabilityMap
			
			val tolerance = 1e-4
			def equal(p: Probability, expected: Double) = p.value must beCloseTo(expected, tolerance)

			equal(map1(4), 0.3333) and 
			equal(map1(5), 0.3333) and
			equal(map1(6), 0.3333)
		}
		
		"sample uniformly by observation index" in {
			val rand = mock[Random]
			
			rand.nextInt(3) returns 1
			rand.nextInt(7) returns 6
			
			(d1.sample(rand) mustEqual 5) and
			(d3.sample(rand) mustEqual 4)
		}
		
		"have map of counts for each observation" in {
			(d1.counts mustEqual Map(4 -> 1, 5 -> 1, 6 ->1)) and
			(d3.counts mustEqual Map(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 1))
		}
		
		"map over observations" in {
			d1.map((a: Int) => a + 1).counts mustEqual Map(
					5 -> 1, 
					6 -> 1, 
					7 -> 1
			)
		}
		
		"be addable" in {
			 d1.+(d2).counts mustEqual Map(
					 4 -> 2, 
					 5 -> 3, 
					 6 -> 4
			)
		}
		
		"be augmentable with TraversableOnce of samples" in {
			val toAdd: TraversableOnce[Int] = List(5,6)
			
			d1.++(toAdd).counts mustEqual Map(
					4 -> 1, 
					5 -> 2, 
					6 -> 2
			)
		}
		
		"calculate inclusive right tail probability" in {
			(d1.rightTail(5).value must beCloseTo(0.66666666, 1e-8)) and
			(d2.rightTail(6).value mustEqual 0.5) and
			(d3.rightTail(5).value mustEqual 0)
		}
		
		"calculate quantile which doesn't land directly on an index" in {
			val instance = FrequencyTable[Double](IndexedSeq(1, 2,2, 3,3,3, 4))
			implicit def asProb(d: Double) = Probability(d)
			
			"Prob"	|  "Quantile"	|
			0.0		!	1			|
			0.25	!	2			|
			0.5		!	3			|
			0.75	!	3			|
			1.0		!	4			|> {
				(p,q) => instance.quantile(p) mustEqual q
			}
		}
		
		"calculate quantile which lands directly on an index" in {
			val instance = FrequencyTable[Double](IndexedSeq(1,2,3,4,5,6,7,8,9,10))
			
			implicit def asProb(d: Double) = Probability(d)
			
			"Prob"	|  "Quantile"	|
			0.0		!	1			|
			0.2		!	2.5			|
			1.0		!	10			|> {
				(p,q) => instance.quantile(p) mustEqual q
			}
		}

		"can identify if distributions are equal if contents are identical using canEqual" in {
			val d4 = FrequencyTable[Int](IndexedSeq(4,5,6))
			
			d1.canEqual(d4) must beTrue
		}
		
		"Override equals and hashcode" in {
			val instance1a = FrequencyTable[Int](IndexedSeq(4, 5))
			val instance1b = FrequencyTable[Int](IndexedSeq(4, 5))
			val instance2 = FrequencyTable[Int](IndexedSeq(4, 5,5))
			
			(instance1a mustEqual instance1b) and
			(instance1a mustNotEqual instance2) and
			(instance1a.hashCode mustEqual instance1b.hashCode) and
			(instance1a.hashCode mustNotEqual instance2.hashCode)
		}
		
		"filter to at least 5" in {
			val instance = d2.filter(_ >= 5)
			
			instance.probabilityMap mustEqual Map(
					5 -> Probability(0.4), 
					6 -> Probability(0.6)
			)
		}
	}
}