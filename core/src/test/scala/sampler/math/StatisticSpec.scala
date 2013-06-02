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

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.junit.runner.RunWith
import sampler.data.Empirical._
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StatisticSpec extends Specification with Mockito with StatisticsComponent {
  implicit val r = Random
  
	"Statistic" should {
		"calculate the mean" in {
		  
		  "EmpiricalSeq" in {
		    val empSeq = IndexedSeq[Double](1,2,3,4).toEmpiricalSeq
		    
		    mean(empSeq) mustEqual 2.5
		  }
		  
		  "EmpiricalTable" in {
		    val empTable = IndexedSeq[Double](1,2,3,4).toEmpiricalTable
		    
		    mean(empTable) mustEqual 2.5
		  }
		  
		  "EmpiricalWeighted" in {
		    val empWeight = Map[Double, Double](
		        (1, 0.25),
		        (2, 0.25),
		        (3, 0.25),
		        (4, 0.25)
		      ).toEmpiricalWeighted
		    
		    mean(empWeight) mustEqual 2.5
		  }
		}
	}
}
