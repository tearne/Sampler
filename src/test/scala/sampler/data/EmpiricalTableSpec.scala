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
import sampler.math._
import scala.math.Fractional
import org.specs2.matcher.DataTables
import sampler.data.Empirical._

@RunWith(classOf[JUnitRunner])
class EmpiricalTableSpec extends Specification with Mockito with DataTables{
  implicit val r = Random

	val once = 1
	val twice = 2
	val thrice = 3
	
	//	--d1--		--d2--		---d3---
	//					6		    3
	//				  5,6		  2,3
	//	4,5,6		4,5,6		1,2,3,4
	val d1 = IndexedSeq(4, 5, 6).toEmpiricalTable
	val d2 = IndexedSeq(4, 5,5, 6,6,6).toEmpiricalTable
	val d3 = IndexedSeq(1, 2,2, 3,3,3, 4).toEmpiricalTable
	
	"EmpiricalTable" should {
		"know the size of its support" in {
			(d1.supportSize mustEqual 3) and
			(d2.supportSize mustEqual 3) and
			(d3.supportSize mustEqual 4)
		}
		
		"calculate probability as relative frequency" in {
			val map1 = d1.probabilities
			
			val tolerance = 1e-4
			def equal(p: Probability, expected: Double) = p.value must beCloseTo(expected, tolerance)

			equal(map1(4), 0.3333) and 
			equal(map1(5), 0.3333) and
			equal(map1(6), 0.3333)
		}
		
		"sample uniformly by observation index" in {
		    var listOfSamples: List[Int] = List()
		  
		    for(i <- 0 until 1000)
		      listOfSamples = listOfSamples.+:(d1.sample)
		  
		    (listOfSamples.count(_ ==4) must beBetween(250, 400)) and
		    (listOfSamples.count(_ ==5) must beBetween(250, 400)) and
		    (listOfSamples.count(_ ==6) must beBetween(250, 400))
		}
		
		"have map of counts for each observation" in {
			(d1.counts mustEqual Map(4 -> 1, 5 -> 1, 6 ->1)) and
			(d3.counts mustEqual Map(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 1))
		}
		
		"until, filter, map, flatmap, combine, convolve, crossCorrelate should all return samplable" in todo
		
		
		//TODO has this not been completed yet?
		//TODO review whether this lot should be in Samplable
		/*
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
		
		"filter to at least 5" in {
			val instance = d2.filter(_ >= 5)
			
			instance.probabilityMap mustEqual Map(
					5 -> Probability(0.4), 
					6 -> Probability(0.6)
			)
		}
		*/
		
		"be augmentable with TraversableOnce of samples" in {
			val toAdd: TraversableOnce[Int] = List(5,6)
			
			d1.++(toAdd).counts mustEqual Map(
					4 -> 1, 
					5 -> 2, 
					6 -> 2
			)
		}
		
		"Override equals and hashcode" in {
			val instance1a = IndexedSeq(4, 5).toEmpiricalTable
			val instance1b = IndexedSeq(4, 5).toEmpiricalTable
			val instance2 = IndexedSeq(4, 5,5).toEmpiricalTable
			
			(instance1a mustEqual instance1b) and
			(instance1a mustNotEqual instance2) and
			(instance1a.hashCode mustEqual instance1b.hashCode) and
			(instance1a.hashCode mustNotEqual instance2.hashCode)
		}
		
		"must not be equal to an Empirical weighted" in {
			val instance1a = IndexedSeq(4, 5).toEmpiricalTable
			val instance1b = Map(4->0.5,5->0.5).toEmpiricalWeighted
			
			instance1a mustNotEqual instance1b
		}
		

	}
}
