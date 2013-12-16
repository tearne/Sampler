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

package sampler.abc

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import sampler.math.Statistics
import sampler.abc.builder.PopulationBuilder
import sampler.math.Random
import sampler.abc.generation.IntegerModel
import org.junit.Before
import sampler.abc.generation.IntegerModel
import sampler.abc.parameters.JobParameters
import sampler.abc.parameters.AlgorithmParameters
import sampler.abc.parameters.ABCParameters

class ABCMethodTest extends AssertionsForJUnit with MockitoSugar{

  val model = IntegerModel
  
  val instance = new ABCMethod{
	val initialise = mock[Initialise]
	val iterate = mock[Iterate]
	val statistics = mock[Statistics]
  }
  
  val jobParams = mock[JobParameters]
  val algorithmParams = mock[AlgorithmParameters]
  val abcParams = ABCParameters(jobParams, algorithmParams)
  val pBuilder = mock[PopulationBuilder]
  val r = mock[Random]

  val ePop = EncapsulatedPopulation(model)(Nil)
  
  @Test def initialisesAndIteratesToGiveOptionPopulation {
    import model._
    val p1 = mock[ParameterSet]
    val p2 = mock[ParameterSet]
    val p3 = mock[ParameterSet]
 	val expectedReturn = Some(Seq(p1, p2, p3))
    
    val w1 = Weighted(Scored(p1, Nil), 10.0)
    val w2 = Weighted(Scored(p2, Nil), 15.0)
    val w3 = Weighted(Scored(p3, Nil), 20.0)

    val ePop2 = EncapsulatedPopulation(model)(Seq(w1, w2, w3))
		
    when(instance.initialise.apply(model, jobParams)).thenReturn(ePop)
    when(instance.iterate.apply(ePop, abcParams, pBuilder, r)).thenReturn(Some(ePop2))
	
    val evolvedPopulation = instance.apply(model, abcParams, pBuilder, r)

    assert(expectedReturn === evolvedPopulation)
  }
  
  @Test def returnsNoneWhenANoneParticleIsGiven {
    when(instance.initialise.apply(model, jobParams)).thenReturn(ePop)
    
    when(instance.iterate.apply(ePop, abcParams, pBuilder, r)).thenReturn(None)
    
    assert(instance.apply(model, abcParams, pBuilder, r) === None)
  }
  
  @Test def ensureTwoParameterMocksAreDifferent {
    val p1 = mock[IntegerModel.ParameterSet]
    
    assert(p1 != mock[IntegerModel.ParameterSet])
  }
}