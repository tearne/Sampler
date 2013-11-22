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

import sampler.abc.ABCModel
import sampler.data.Distribution
import sampler.abc.Prior

object IntegerModel extends ABCModel {
    case class ParameterSet(i: Int) extends ParameterSetBase with Serializable {
      def perturb() = this
      def perturbDensity(that: ParameterSet) = if(that == this) 1.0 else 0.0
    }

    case class Observed()
    val observed = Observed()
    
    case class Simulated() extends SimulatedBase{
      def distanceToObserved: Double = 1.0
    }
          
    def modelDistribution(p: ParameterSet) = new Distribution[Simulated] with Serializable {
      override def sample = Simulated()
    }
      
    val prior = new Prior[ParameterSet] with Serializable{
      val dist = Distribution.continually(1)
      def density(p: ParameterSet) = 1.0
      def sample() = ParameterSet(1)
    }
}