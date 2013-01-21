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

package sampler.examples

import sampler.math.Random
import sampler.data._
import sampler.fit._
import sampler.math._
import sampler.data.Samplable
import sampler.io.CSVTableWriter
import java.nio.file.Paths
import sampler.data.Types.Column
import sampler.run.SerialRunner
import sampler.data.Empirical._

// TODO draw a posterior graph

object SimplerABC extends App {

  implicit val random = new Random()
  
  val wd = Paths.get("examples").resolve("simplerABC")
  
  class Model extends ABCModel[Random]{
    
    // TODO make parameters hold probability of heads
    case class Parameters(c: Boolean) extends ParametersBase {
      def perturb = Parameters(Samplable.coinToss.sample)
      
//      TODO perturb probability of head p with a normal distribution to expose ABC bug
      
      def perturbDensity(that: Parameters) = {
        if(this.c == that.c) 1.0 else 0.0
      }
    }

    case class Observations(x: List[Boolean]) extends ObservationsBase
    
    case class Output(outputValues: List[Boolean]) extends OutputBase {
      
//      TODO may be wrong	
      def closeToObserved(obs: Observations, tolerance: Double): Boolean = {
        def propTrue(y: List[Boolean]) = y.toIndexedSeq.toEmpiricalTable.probabilities(true).value

        math.abs(propTrue(outputValues) - propTrue(obs.x)) < 0.001
      }
    }
    
    def init(p: Parameters, obs: Observations) = new Samplable[Output, Random] {
    	
    	override def sample(implicit r: Random) = {
    	    def f(x: Boolean) = x
    		Output(obs.x.map(f))
//    		Output(List(true, false))
    	}
    }
    
  }
  
  val model = new Model
  import model._
  
  val prior = new Prior[Parameters, Random]{
    def density(p: Parameters) = {
      1.0
    }
    
    def sample(implicit r: Random) = {
      Parameters(r.nextDouble() < 0.5)
    }
  }
  
  val obs = Observations(List(true, false, true, false))
  
	object Writer extends PopulationWriter{
		def apply(p: Seq[Parameters], tolerance: Double){
			new CSVTableWriter(wd.resolve(tolerance+".csv"), true).apply(
				Column(p.map(_.c), "C")
			)
		}
	}
	
	object ABCRunner extends ABCComponent 
				  with SampleBuilderComponent{
		val builder = SerialSampleBuilder
	}
	
    val resultParams = ABCRunner(model, random)(
			prior, 
			obs,
			new ABCParameters(20, 1000, 1000000, 20, 500),
			/*runner,*/new SerialRunner,
			Some(Writer)
	)
}