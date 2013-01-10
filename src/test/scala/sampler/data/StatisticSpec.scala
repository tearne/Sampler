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
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.data.Empirical._
import sampler.math.Statistics
import scala.math.Fractional

@RunWith(classOf[JUnitRunner])
class StatisticSpec extends Specification with Mockito{
	"Statistic" should {
		"calculate the mean" in {
		  
		  "for an EmpiricalSeq" in {
		    val empSeq = IndexedSeq(1,2,3,4).toEmpiricalSeq
		    
		    val stats = new Statistics
		    
//		    TODO not locating implicit Fractional
//		    stats.mean(empSeq)
		  }
		  
		  "for an EmpiricalTable" in {
		    val empTable = IndexedSeq(1,2,3,4).toEmpiricalTable
		    
		    val stats = new Statistics
		    
//		    TODO not locating implicit Fractional
//		    stats.mean(empTable)
		  }
		  
		  "for an EmpiricalWeighted" in {
		    val empWeight = Map(
		        1 -> 0.25,
		        2 -> 0.25,
		        3 -> 0.25,
		        4 -> 0.25
		      ).toEmpiricalWeighted
		    
		    val stats = new Statistics
		    
//		    TODO not locating implicit Fractional
//		    stats.mean(empWeight)
		  }
		  
//			val empirical = mock[Empirical[Double]]
//			val counts = List((1.1,10), (2.2,20), (3.3, 30)).toMap
//			empirical.counts returns counts
//			empirical.size returns 60
//			
//			Statistic.mean(empirical) mustEqual (11+44+99)/60.0
		}
	}
}