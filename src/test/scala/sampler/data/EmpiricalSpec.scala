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

@RunWith(classOf[JUnitRunner])
class FrequencyTableSpec extends Specification with Mockito{
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
	
	"Frequency tables" should {
		
		"return the number of items in the sequence used to create the table when asked for the size" in {
			(d1.size mustEqual 3) and
			(d2.size mustEqual 6) and
			(d3.size mustEqual 7)
		}
		
		"calculate the probability of observing each sample" in {
			val map1 = d1.probabilityMap
			
			def closeTo(p: Probability, expected: Double) = {
				val tolerance = 1e-4
				
				p.value must beCloseTo(expected, tolerance)
			}

			closeTo(map1(4), 0.3333)
			closeTo(map1(5), 0.3333)
			closeTo(map1(6), 0.3333)
		}
		
		"select a random element from the table when a sample is requested" in {
			val rand = mock[Random]
			
			rand.nextInt(3) returns 1
			rand.nextInt(7) returns 6
			
			(d1.sample(rand) mustEqual 5) and
			(d3.sample(rand) mustEqual 4)
		}
		
		"produce a map of counts of each observation" in {
			val m1 = d1.counts
			
			val m3 = d3.counts
			
			(m1(4) mustEqual 1) and
			(m1(5) mustEqual 1) and
			(m1(6) mustEqual 1) and
			(m1.get(7) mustEqual None) and
			(m3(1) mustEqual 1) and
			(m3(2) mustEqual 2) and
			(m3(3) mustEqual 3) and
			(m3(4) mustEqual 1)
		}
		
		"be able to correctly map over the values" in {
		
			val d4 = d1.map((a: Int) => a + 1)
			
			val m4 = d4.counts
			
			(m4.get(4) mustEqual None) and
			(m4(5) mustEqual 1) and
			(m4(6) mustEqual 1) and
			(m4(7) mustEqual 1)
		}
		
		"be able to have an extra sample added once initial table has been created" in {
			val newTable = d1.+(1)
			newTable.samples mustEqual List(4,5,6,1)
		}
		
		"be able to combine into one big frequency table" in {
			val combined = d1.+(d2)
			
			(combined.size mustEqual 9) and
			(combined.counts(4) mustEqual 2) and
			(combined.counts(5) mustEqual 3) and
			(combined.counts(6) mustEqual 4)
		}
		
		"be able to add a list of samples?" in {
			val combined = d1.++(List(5,6))
			
			val m1 = combined.counts
			
			(combined.size mustEqual(5)) and
			(m1(4) mustEqual 1) and
			(m1(5) mustEqual 2) and
			(m1(6) mustEqual 2)
		}
		
		"have a working right tail" in {
			(d1.rightTail(5).value must beCloseTo(0.66666666, 1e-8)) and
			(d2.rightTail(6).value mustEqual 0.5) and
			(d3.rightTail(5).value mustEqual 0)
		}
		
		"have a working quantile where value is selected from the index above the calculation that doesn't land directly on an index" in {
			
			val d4 = FrequencyTable[Double](IndexedSeq(1, 2,2, 3,3,3, 4))
			
			(d4.quantile(Probability(0.0)) mustEqual 1) and
			(d4.quantile(Probability(0.25)) mustEqual 2) and
			(d4.quantile(Probability(0.50)) mustEqual 3) and
			(d4.quantile(Probability(0.75)) mustEqual 3) and
			(d4.quantile(Probability(1.00)) mustEqual 4)
		}
		
		"have a quantile function where the result is a mean of two entries if calculation lands directly on an index" in {
			val d4 = FrequencyTable[Double](IndexedSeq(1,2,3,4,5,6,7,8,9,10))
			
			(d4.quantile(Probability(0.0)) mustEqual 1) and
			(d4.quantile(Probability(0.2)) mustEqual 2.5) and
			(d4.quantile(Probability(1.0)) mustEqual 10)
		}

		"can identify if distributions are equal if contents are identical using canEqual" in {
			val d4 = FrequencyTable[Int](IndexedSeq(4,5,6))
			
			d1.canEqual(d4) must beTrue
		}
		
		"be able to calculate the distance between distribtions" in { 
			todo			
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
	}
}