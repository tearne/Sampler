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
	
	val p1 = Particle(1, 1.25)
	val p2 = Particle(2, 1.25)
	val p3 = Particle(3, 1.25)
	val p4 = Particle(4, 1.25)
	
	val particleSeq = IndexedSeq(p1,p2,p3,p4)
	
	val w1 = WeightsTable(particleSeq)
	def allPass[T](results: IndexedSeq[MatchResult[T]]) = results.reduceLeft{(a,b) => a and b}
	
	"Weights table" should {
		"have table size" in {
			w1.size mustEqual 4
		}
		
		"normalised weights" in {
			w1.normalised.map(_.weight).sum mustEqual 1
		}
		
		"map each entry to a probability object" in {
			val probMap = w1.probabilityMap
			
			val quarter = Probability(0.25)
			probMap mustEqual Map(
					1 -> quarter,
					2 -> quarter,
					3 -> quarter,
					4 -> quarter
			)
		}
		
		"sample distribution (using the alias method as default)" in {
		  
		  val rand = new Random
		  
		  var listOfSamples: List[Int] = List()
		  
		  for(i <- 0 until 1000)
		    listOfSamples = listOfSamples.+:(w1.sample(rand).value)
		  
		  (listOfSamples.count(_ ==1) must beBetween(200, 300)) and
		  (listOfSamples.count(_ ==2) must beBetween(200, 300)) and
		  (listOfSamples.count(_ ==3) must beBetween(200, 300)) and
		  (listOfSamples.count(_ ==4) must beBetween(200, 300))
		}
		
		"be convertable to frequency table" in {
			val freqTable: FrequencyTable[Int] = w1.discardWeights
			
			freqTable.counts mustEqual Map(
					1 -> 1, 
					2 -> 1, 
					3 -> 1, 
					4 -> 1
			)
		}
		
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
		
		"override equals and hashcode" in {
			val instance1a = WeightsTable(particleSeq)
			val instance1b = WeightsTable(particleSeq)
			val instance2 = WeightsTable(Seq(p1,p2,p3,p4, Particle(5,1.25)))
			
			(instance1a mustEqual instance1b) and
			(instance1a mustNotEqual instance2) and
			(instance1a.hashCode mustEqual instance1b.hashCode) and
			(instance1a.hashCode mustNotEqual instance2.hashCode)
		}
		
		"possible repetition" in {
		  val a1 = Particle(1, 1.25)
		  val a2 = Particle(2, 1.25)
		  val a3 = Particle(1, 2.50)
		  
		  val repMap = WeightsTable(IndexedSeq(a1, a2, a3))
		  
		  val probMap = repMap.probabilityMap
		  
		  (probMap.get(1).get.value mustEqual 0.75) and
		  (probMap.get(2).get.value mustEqual 0.25)
		}
	}
}