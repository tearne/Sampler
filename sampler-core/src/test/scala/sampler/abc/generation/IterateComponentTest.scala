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

package sampler.abc.generation

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import sampler.abc.builder.PopulationBuilder
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import sampler.abc.ABCParameters
import sampler.io.Logging
import sampler.math.Random
import sampler.abc.EncapsulatedPopulation
import sampler.abc.ABCParameters
import sampler.math.StatisticsComponent
import sampler.math.Statistics

class RunComponentTest extends AssertionsForJUnit with MockitoSugar{
  val anything = 0
  val zeroGenerations = 0
	
  val abcParams = ABCParameters(
    anything, 
	anything, 
	anything, 
	zeroGenerations, 
	anything, 
	anything
  )
  
  val random = Random
  val p0 = EncapsulatedPopulation(VacuousModel)(Nil)
	
  @Test
  def runReturnsInitialPopluationWhenRefinementsIsZero {
    val instance = new IterateComponent with Logging with StatisticsComponent{
		val iterate = new Iterate{}
		val statistics = mock[Statistics]
    }
    
  	val populationBuilder = mock[PopulationBuilder]
    
    val result = instance.iterate(p0, abcParams, populationBuilder, random).get
    assert(result === p0)
  }
}