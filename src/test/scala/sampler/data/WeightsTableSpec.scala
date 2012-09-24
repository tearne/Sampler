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
		
		"tidy exception in normalisation function" in todo
		
		"have cumulative weights" in {
			w1.cumulativeWeights mustEqual IndexedSeq(0.25, 0.5, 0.75, 1.0)
		}
		
		"map each entry to a probability object" in {
			/* 	NOTE FOR OLIVER
			 * 
			 * 	At this point the Particle object for each entry contains the probability
			 *  as it's weight parameter, so why the need to map against probability??
			 */
			
			// I'm not aware that we have any other method for mapping from value to particle, have we?
			
			val probMap = w1.probabilityMap
			
			val quarter = Probability(0.25)
			probMap mustEqual Map(
					1 -> quarter,
					2 -> quarter,
					3 -> quarter,
					4 -> quarter
			)
		}
		
		"returns the values of the probability map ??? " in todo
		
		"sample by cumulative weight" in {
			val rand = mock[Random]
			rand.nextDouble() returns 0.3
			
			w1.sample(rand) mustEqual Particle(2,0.25)
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
		
		"be mappble" in {
//			don't understand the A -> B conversion
//			w1.map(a => println(a))
			todo
		}
		
		"be able to detect if two weights tables are equal" in {
			val w2 = WeightsTable(particleSeq)
			
			w1.equals(w2) mustEqual true
		}
		
		"override equals and hashcode" in todo
	}
}