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

import sampler.abc.ABCParameters
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import sampler.abc.Particle
import sampler.abc.ABCModel
import sampler.data.Distribution
import sampler.abc.Prior

class InitialiseComponentTest extends AssertionsForJUnit with MockitoSugar{
  
  object VacuousModel1 extends ABCModel {
    case class Parameters() extends ParametersBase with Serializable {
      def perturb() = this
      def perturbDensity(that: Parameters) = if(that == this) 1.0 else 0.0
    }

    case class Observed()
    val observed = Observed()
    
    case class Simulated() extends SimulatedBase{
      def distanceToObserved: Double = 1.0
    }
          
    def modelDistribution(p: Parameters) = new Distribution[Simulated] with Serializable {
      override def sample = Simulated()
    }
      
    val prior = new Prior[Parameters] with Serializable{
      val dist = Distribution.continually(1)
      def density(p: Parameters) = 1.0
      def sample() = Parameters()
    }
  }
  
  val anything = 1 
  val fiveParticles = 5
	
  val abcParams = ABCParameters(
    anything, 
	fiveParticles, 
	anything, 
	anything, 
	anything, 
	anything
  )
	
  @Test
  def testInitialisation {
  	val instance = new InitialiseComponent{
  		val initialise = new Initialise{}
  	}
  	
  	val model = VacuousModel1
  	
    val ePop0 = instance.initialise(model, abcParams)
    val pop0 = ePop0.population
    
    assert(pop0.length === 5)
    val expectedParticle = Particle(model.Parameters(), 1.0, Double.MaxValue)
    pop0.foreach(a => assert(a === Particle(model.Parameters(), 1.0, Double.MaxValue)))
  }
}