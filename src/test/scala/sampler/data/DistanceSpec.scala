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
import org.specs2.mock.Mockito
import sampler.data.Empirical._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DistanceSpec extends Specification with Mockito with EmpiricalMetricComponent{
	"Empirical Metric Component" should {
		"implement an absolute difference metric" in {
			val instance1 = IndexedSeq[Double](1,2,3).toEmpiricalSeq // mean 2
			val instance2 = IndexedSeq[Double](4,5,6).toEmpiricalSeq // mean 5
			
			metric.absoluteMean(instance1, instance2) mustEqual 3
		}
		
		"implement a maximum difference metric" in {
			val instance1 = IndexedSeq(1,2,3,4).toEmpiricalSeq 
			val instance2 = IndexedSeq(1,2,2,2).toEmpiricalSeq // biggest distance 4
			
			metric.max(instance1, instance2) mustEqual 0.5
		}
	}
}