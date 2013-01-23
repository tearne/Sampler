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
import scala.annotation.tailrec

// TODO draw a posterior graph
// TODO there may be an error stopping the results converging on the correct probability

object SimplerABC extends App {

  implicit val random = new Random()
  
  val wd = Paths.get("examples").resolve("simplerABC")
  val max = 1.0
  val min = 0.0
  
  class Model extends ABCModel[Random]{
    
    case class Parameters(p: Double) extends ParametersBase {
      val perturbationDist = Samplable.normal(0,1)
      
      @tailrec
      final def perturb(d: Double): Double = {
        val candidate = d + perturbationDist.sample
        if(candidate <= max && candidate >= min) candidate
        else perturb(d)
      }
      
      def perturb() = Parameters(perturb(p))
      
      def perturbDensity(that: Parameters) = {
        perturbationDist.density(p - that.p)
      }
    }

    case class Observations(x: List[Double]) extends ObservationsBase
    
    case class Output(outputValues: List[Double]) extends OutputBase {
      
      def closeToObserved(obs: Observations, tolerance: Double): Boolean = {
        val distance = outputValues.zip(obs.x).foldLeft(0.0){case (acc,(s,o)) =>
			acc + math.pow(s - o, 2.0)
		}
        distance < tolerance
      }
    }
    
    def init(p: Parameters, obs: Observations) = new Samplable[Output, Random] {
    	val modelNoiseDist = Samplable.normal(0,p.p).map(s => s*s)
		override def sample(implicit r: Random) = {
			def f(x: Double) = p.p * x + math.sqrt(x) * modelNoiseDist.sample(r)
			Output(obs.x.map(f))
		}
    }
    
  }
  
  val model = new Model
  import model._
  
  val prior = new Prior[Parameters, Random]{
    def density(p: Parameters) = {
      if(p.p > max || p.p < min) 0.0
      else 1.0 / ((max - min)*(max - min))
    }
    
    def sample(implicit r: Random) = {
      Parameters(r.nextDouble(min, max))
    }
  }
  
  val obs = Observations(List(0.45, 0.5, 0.55, 0.5))
  
	object Writer extends PopulationWriter{
		def apply(p: Seq[Parameters], tolerance: Double){
			new CSVTableWriter(wd.resolve(tolerance+".csv"), true).apply(
				Column(p.map(_.p), "P")
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
			new ABCParameters(20, 1000, 1, 20, 500),
			/*runner,*/new SerialRunner,
			Some(Writer)
	)
}