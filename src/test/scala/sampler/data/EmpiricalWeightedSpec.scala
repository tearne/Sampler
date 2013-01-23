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

import sampler.data.Empirical._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.math.Probability
import org.specs2.mock.Mockito
import sampler.math.Random
import org.specs2.matcher.MatchResult
import sampler.math.Random

@RunWith(classOf[JUnitRunner])
class WeightsTableSpec extends Specification with Mockito{
	
	val p1 = (1, 1.25)
	val p2 = (2, 1.25)
	val p3 = (3, 1.25)
	val p4 = (4, 1.25)
	
	val weightedValues = Map(
			1 -> 1.25,
			2 -> 1.25,
			3 -> 1.25,
			4 -> 1.25
	)
	
	val w1 = weightedValues.toEmpiricalWeighted
	def allPass[T](results: IndexedSeq[MatchResult[T]]) = results.reduceLeft{(a,b) => a and b}
	
	"EmpiricalWeighted" should {
		"have table size" in {
			w1.supportSize mustEqual 4
		}
		
		"map each entry to a probability object" in {
			val probMap = w1.probabilities
			
			val quarter = Probability(0.25)
			probMap mustEqual Map(
					1 -> quarter,
					2 -> quarter,
					3 -> quarter,
					4 -> quarter
			)
		}
		
		"sample distribution (using the alias method as default)" in {
		  
		  implicit val rand = new Random
		  
		  var listOfSamples: List[Int] = List()
		  
		  for(i <- 0 until 1000)
		    listOfSamples = listOfSamples.+:(w1.sample)
		  
		  (listOfSamples.count(_ ==1) must beBetween(200, 300)) and
		  (listOfSamples.count(_ ==2) must beBetween(200, 300)) and
		  (listOfSamples.count(_ ==3) must beBetween(200, 300)) and
		  (listOfSamples.count(_ ==4) must beBetween(200, 300))
		}
		
		"be convertable to empirical table" in {
			val freqTable: EmpiricalTable[Int] = w1.toEmpiricalTable
			
			freqTable.counts mustEqual Map(
					1 -> 1, 
					2 -> 1, 
					3 -> 1, 
					4 -> 1
			)
		}
		
		//TODO
		"be convertalbe to empirical sequency" in todo
		
		//TODO has this not be completed yet?
		//TODO consider whether to move this lot into Samplable
		/*
		"filter particles in the table" in {
			val filtered = w1.filter(_.value > 2)
			
			val normalised = filtered.normalised
			
			(normalised.size mustEqual 2) and
			(normalised(0) mustEqual Particle(3, 0.5)) and
			(normalised(1) mustEqual Particle(4, 0.5))
		}
		
		"be mappable to a new weights table, ratio of weights remain the same after transformation" in {
			val newMap = w1.map((a: Particle[Int]) => Particle(a.value, a.weight*2))
			
			val probMap = newMap.probabilityMap
			
			(probMap.get(1).get.value mustEqual 0.25) and
			(probMap.get(2).get.value mustEqual 0.25) and
			(probMap.get(3).get.value mustEqual 0.25) and
			(probMap.get(4).get.value mustEqual 0.25)
		}
		*/
		
		"override equals and hashcode" in {
			val instance1a = weightedValues.toEmpiricalWeighted
			val instance1b = weightedValues.toEmpiricalWeighted
			val instance2 = weightedValues + (5 -> 1.25) toEmpiricalWeighted
			
			(instance1a mustEqual instance1b) and
			(instance1a mustNotEqual instance2) and
			(instance1a.hashCode mustEqual instance1b.hashCode) and
			(instance1a.hashCode mustNotEqual instance2.hashCode)
		}
		
		"must not be equal to EmpiricalTable" in {
		  val instance1a = weightedValues.toEmpiricalWeighted
		  val instance1b = IndexedSeq(1,2,3,4).toEmpiricalTable
		  
		  instance1a mustNotEqual instance1b
		}
	}
}