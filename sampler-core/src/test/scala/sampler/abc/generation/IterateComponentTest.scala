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

import org.junit.Test
import org.mockito.Mockito.when
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

import sampler.abc.ABCParameters
import sampler.abc.EncapsulatedPopulation
import sampler.abc.Particle
import sampler.abc.builder.PopulationBuilder
import sampler.io.Logging
import sampler.math.Random
import sampler.math.Statistics
import sampler.math.StatisticsComponent

class IterateComponentTest extends AssertionsForJUnit with MockitoSugar {
  val anything = 0
	
  val parameters = ABCParameters(
    anything, anything, anything,
    anything, anything, anything
  )
  
  val instance = new IterateComponent with Logging with StatisticsComponent{
	val iterate = new Iterate{}
	val statistics = mock[Statistics]
  }
  
  val random = Random
  val populationBuilder = mock[PopulationBuilder]

  val pop0 = EncapsulatedPopulation(IntegerModel)(Nil)

  @Test
  def runReturnsInitialPopluationWhenRefinementsIsZero {
	val abcParams = parameters.copy(refinements = 0)
    
    val result = instance.iterate(pop0, abcParams, populationBuilder, random).get
    assert(result === pop0)
  }
  
  @Test def oneRefinementUnsuccessfulRefinementSoReturnsInitialPopulation {
    val tolerance = 1e6
    val refinements = 1
    
    val abcParams = parameters.copy(
      startTolerance = tolerance,
      refinements = refinements
    )
    
    when(populationBuilder.run(pop0, abcParams, tolerance, random)).thenReturn(None)
    
    val result = instance.iterate(pop0, abcParams, populationBuilder, random).get
    
    assert(result === pop0)
  }
  
  @Test def oneRefinementSuccessfulRefinementReturnsNewPopulation {
  	val tolerance = 1e6
  	val refinements = 1
  	
  	val abcParams = parameters.copy(
  	    startTolerance = tolerance,
  	    refinements = refinements
  	)
  	
    val p1 = new Particle(mock[IntegerModel.Parameters], 1, Double.MaxValue)
    
  	val pop1 = EncapsulatedPopulation(IntegerModel)(Seq(p1))
  	
    when(populationBuilder.run(pop0, abcParams, tolerance, random)).thenReturn(Some(pop1))
    
    val result = instance.iterate(pop0, abcParams, populationBuilder, random).get
    
    assert(result === pop1)
  }
  
  @Test def twoRefinementsImplicitelyCorrectToleranceOnSecondLoop {
  	val refinements = 2
  	val tolerance = 1e6
  	val tolerance2 = 500000.0
  	
  	val abcParams = parameters.copy(
  	    startTolerance = tolerance,
  	    refinements = refinements
  	)
    
    val p1 = new Particle(mock[IntegerModel.Parameters], 1, Double.MaxValue)
  	val p2 = new Particle(mock[IntegerModel.Parameters], 1, Double.MaxValue)
    
  	val pop1 = EncapsulatedPopulation(IntegerModel)(Seq(p1))
  	val pop2 = EncapsulatedPopulation(IntegerModel)(Seq(p2))
  	
  	when(populationBuilder.run(pop0, abcParams, tolerance, random)).thenReturn(Some(pop1))
  	when(populationBuilder.run(pop1, abcParams, tolerance2, random)).thenReturn(Some(pop2))
    
  	val result = instance.iterate(pop0, abcParams, populationBuilder, random).get
  	
    assert(result === pop2)
  }
  
  @Test def scenarioWhereMedianMeanFitInstZero {
    fail()
  }
}