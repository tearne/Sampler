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
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
import sampler.math._

@RunWith(classOf[JUnitRunner])
class EmpiricalSeqSpec extends Specification{
  implicit val rs = new RandomSourceImpl {}

	val seq1 = IndexedSeq(1,2,3,4)
  
	val es1 = seq1.toEmpiricalSeq
	
	"EmpiricaSeq" should {
	  "have the correct size" in {
	    es1.supportSize mustEqual 4
	  }
	  
	  "give the correct probabilities" in {
	    val probMap = es1.probabilities
	    
	    val tolerance = 1e-4
	    def equal(p: Probability, expected: Double) = p.value must beCloseTo(expected, tolerance)
	    
	    (equal(probMap(1), 0.25)) and
	    (equal(probMap(2), 0.25)) and
	    (equal(probMap(3), 0.25)) and
	    (equal(probMap(4), 0.25))
	  }
	  
	  "be able to be added to" in {
	    val seq2 = IndexedSeq(5,6,7,8)
	    
	    val es2 = es1.++(seq2)
	    
	    es2.supportSize mustEqual 8
	  }
	}
}
