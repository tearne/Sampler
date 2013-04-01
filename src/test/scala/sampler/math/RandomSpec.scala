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
 * See the Lcense for the specific language governing permissions and
 * limitations under the License.
 */

package sampler.math;

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RandomSpec extends Specification{
	"RandomSpec" should {
	  
	  val random = Random
	  
		"generate integers" in {
		  
		  "of a number in the correct range" in {
		    val draw1 = random.nextInt(5)
		    
		    draw1 must beBetween(0,4)
		  }
		  
		  "where selections are evenly distributed across the range" in {
			def sample(samples: List[Int], currentIt: Int, numIts: Int): List[Int] = {
			  if(currentIt>=numIts) samples
			  else {
				  sample(samples.:+(random.nextInt(4)), currentIt+1, numIts)
			  }
			}
		    
			val requiredIterations = 1000
					
		    val sampledInts = sample(List(), 0, requiredIterations)
		    
		    (sampledInts.count(_ == 0) must beBetween(200, 300)) and
		    (sampledInts.count(_ == 1) must beBetween(200, 300)) and
		    (sampledInts.count(_ == 2) must beBetween(200, 300)) and
		    (sampledInts.count(_ == 3) must beBetween(200, 300))
		  }
		  
		}
		
		"generate doubles" in {
		  
		  "in the correct range" in {
		    val draw1 = random.nextDouble(0.5, 2.5)
		    val draw2 = random.nextDouble(1.5, 2.5)
		    val draw3 = random.nextDouble(3.0, 5.0)
		    
		    (draw1 must beBetween(0.5,2.5)) and
		    (draw2 must beBetween(1.5,2.5)) and
		    (draw3 must beBetween(3.0,5.0))
		  }
		}
		
		"generate booleans" in {
		  
		  "with the correct ratio of true/false" in {
		    def booleanSample(samples: List[Boolean], p: Probability, currentIt: Int, numIts: Int): List[Boolean] = {
		      if(currentIt >= numIts) samples
		      else {
		        booleanSample(samples.:+(random.nextBoolean(p)), p, currentIt+1, numIts)
		      }
		    }
		    
		    val probability = new Probability(0.9)
		    val requiredIterations = 1000
		    
		    val sampledBooleans = booleanSample(List(), probability, 0, requiredIterations)
		    
		    (sampledBooleans.count(_ == true) must beBetween(850, 950)) and
		    (sampledBooleans.count(_ == false) must beBetween(50, 150))
		  }
		}
	}
}