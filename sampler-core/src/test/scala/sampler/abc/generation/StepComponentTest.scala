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
import org.scalatest.mock.MockitoSugar._
import org.mockito.Mockito.when
import sampler.math.Random
import sampler.abc.population.PopulationBuilder
import sampler.io.Logging
import sampler.abc.ABCMethod
import sampler.abc.ABCParameters
import sampler.abc.population.EncapsulatedPopulation
import sampler.abc.Particle
import scala.util.Success
import org.junit.Before
import scala.util.Failure
import sampler.abc.MaxRetryException

class StepComponentTest extends AssertionsForJUnit {
  val reps = 1 
  val particles = 2
  val tolerance = 1e6
  val refinements = 1
  val particleRetries = 1
  val chunkSize = 1
	
  val abcParams = ABCParameters(
    reps, 
	particles, 
	tolerance, 
	refinements, 
	particleRetries, 
	chunkSize
  )
  
  val p0 = EncapsulatedPopulation(VacuousModel)(Nil)
  val particle1 = Particle[p0.model.Parameters](null, 0.1,100)
  val particle2 = Particle[p0.model.Parameters](null, 0.2,200)
  val particle3 = Particle[p0.model.Parameters](null, 0.3,300)
  val random = Random
  
  val instance = new StepComponent with Logging {
  		val step = new Step{}
  }

  @Test def populationEvolvesSuccessfully{
  	val popBuilder = mock[PopulationBuilder]
  	
  	when(popBuilder.run(p0, Seq(1,1), tolerance, abcParams, random)).thenReturn(
  		Seq(
  			Success(EncapsulatedPopulation(p0.model)(Seq(particle1))),
  			Success(EncapsulatedPopulation(p0.model)(Seq(particle2)))
  		)
  	)
  	
  	val result = instance.step(
  			p0, 
  			popBuilder,
  			abcParams,
  			tolerance,
  			random
  	).get
  	
  	assert(result.population === Seq(particle1,particle2))
  }
  
  @Test def returnsNoneIfRefinementAbortionException{
  	val popBuilder = mock[PopulationBuilder]
  	
  	when(popBuilder.run(p0, Seq(1,1), tolerance, abcParams, random)).thenReturn(
  		Seq(
  			Success(EncapsulatedPopulation(p0.model)(Seq(particle1))),  			Failure(new MaxRetryException("Bleh"))
  		)
  	)
  	
  	val result = instance.step(
  			p0, 
  			popBuilder,
  			abcParams,
  			tolerance,
  			random
  	)
  	
  	assert(result === None)
  }

  @Test
  def exceptionIfAnyOtherExceptionReturned {
  	val popBuilder = mock[PopulationBuilder]
  	
  	when(popBuilder.run(p0, Seq(1,1), tolerance, abcParams, random)).thenReturn(
  		Seq(
  		    Failure(new MaxRetryException("Bleh")),  			Failure(new ArrayIndexOutOfBoundsException("Bleh"))
  		)
  	)
  	
  	intercept[ArrayIndexOutOfBoundsException]{
    	instance.step(
  			p0, 
  			popBuilder,
  			abcParams,
  			tolerance,
  			random
      )
    }
  }
  
  @Test
  def testCorrectChangingOfToleranceAsRefinementsGoOn {
  	fail("TODO")
  }
}