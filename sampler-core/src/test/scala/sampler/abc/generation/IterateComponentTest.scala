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
import sampler.abc.Particle

class IterateComponentTest extends AssertionsForJUnit with MockitoSugar {
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
  val pop0 = EncapsulatedPopulation(IntegerModel)(Nil)
	
  @Test
  def runReturnsInitialPopluationWhenRefinementsIsZero {
    val instance = new IterateComponent with Logging with StatisticsComponent{
		val iterate = new Iterate{}
		val statistics = mock[Statistics]
    }
    
  	val populationBuilder = mock[PopulationBuilder]
    
    val result = instance.iterate(pop0, abcParams, populationBuilder, random).get
    assert(result === pop0)
  }
  
  @Test def oneRefinementUnsuccessfulRefinementSoReturnsInitialPopulation {
    val instance = new IterateComponent with Logging with StatisticsComponent{
		val iterate = new Iterate{}
		val statistics = mock[Statistics]
    }
    
  	val populationBuilder = mock[PopulationBuilder]
    
  	val oneGeneration = 1
  	val tolerance1 = 1e6
  	
  	val abcParams1 = ABCParameters(
  	  anything, 
  	  anything, 
	  tolerance1, 
	  oneGeneration, 
	  anything,
	  anything
    )
  	
    when(populationBuilder.run(pop0, abcParams1, tolerance1, random)).thenReturn(None)
    
    val result = instance.iterate(pop0, abcParams1, populationBuilder, random).get
    
    assert(result === pop0)
  }
  
  @Test def oneRefinementSuccessfulRefinementReturnsNewPopulation {
    val instance = new IterateComponent with Logging with StatisticsComponent{
		val iterate = new Iterate{}
		val statistics = mock[Statistics]
    }
    
  	val populationBuilder = mock[PopulationBuilder]
    
  	val oneGeneration = 1
  	val tolerance1 = 1e6
  	
  	val abcParams1 = ABCParameters(
  	  anything, 
  	  anything, 
	  tolerance1, 
	  oneGeneration, 
	  anything,
	  anything
    )
  	
    val p1 = new Particle(mock[IntegerModel.Parameters], 1, Double.MaxValue)
  	val p2 = new Particle(mock[IntegerModel.Parameters], 1, Double.MaxValue)
    
  	val pop1 = EncapsulatedPopulation(IntegerModel)(Seq(p1,p2))
  	
    when(populationBuilder.run(pop0, abcParams1, tolerance1, random)).thenReturn(Some(pop1))
    
    val result = instance.iterate(pop0, abcParams1, populationBuilder, random).get
    
    assert(result === pop1)
  }
  
  @Test def twoRefinementsImplicitelyCorrectToleranceOnSecondLoop {
        val instance = new IterateComponent with Logging with StatisticsComponent{
		val iterate = new Iterate{}
		val statistics = mock[Statistics]
    }
    
  	val populationBuilder = mock[PopulationBuilder]
    
  	val twoGenerations = 2
  	val tolerance1 = 1e6
  	val tolerance2 = 500000.0
  	
  	val abcParams2 = ABCParameters(
  	  anything, 
  	  anything, 
	  tolerance1, 
	  twoGenerations, 
	  anything,
	  anything
    )
    
    val p1 = new Particle(mock[IntegerModel.Parameters], 1, Double.MaxValue)
  	val p2 = new Particle(mock[IntegerModel.Parameters], 1, Double.MaxValue)
  	val p3 = new Particle(mock[IntegerModel.Parameters], 1, Double.MaxValue)
  	val p4 = new Particle(mock[IntegerModel.Parameters], 1, Double.MaxValue)
    
  	val pop1 = EncapsulatedPopulation(IntegerModel)(Seq(p1,p2))
  	val pop2 = EncapsulatedPopulation(IntegerModel)(Seq(p3,p4))
  	
  	when(populationBuilder.run(pop0, abcParams2, tolerance1, random)).thenReturn(Some(pop1))
  	when(populationBuilder.run(pop1, abcParams2, tolerance2, random)).thenReturn(Some(pop2))
    
  	val result = instance.iterate(pop0, abcParams2, populationBuilder, random).get
  	
    assert(result === pop2)
  }
  
  @Test def scenarioWhereMedianMeanFitInstZero {
    fail()
  }
}