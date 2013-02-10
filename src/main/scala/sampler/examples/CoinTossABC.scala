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
import sampler.r.QuickPlot

// TODO draw a posterior graph
// TODO there may be an error stopping the results converging on the correct probability

object SimplerABC extends App {
  implicit val random = new Random()
  implicit def toProbability(d: Double) = Probability(d)
  
  val wd = Paths.get("examples").resolve("simplerABC")
  val max = 1.0
  val min = 0.0
  
  class Model extends ABCModel[Random]{
    case class Parameters(pHeads: Double) extends ParametersBase {
      val perturbationKernel = Samplable.normal(0,1)
      
      def perturb(d: Double): Double = d + perturbationKernel.sample
      
      def perturb() = Parameters(perturb(pHeads))
      
      def perturbDensity(that: Parameters) = {
        perturbationKernel.density(pHeads - that.pHeads)
      }
    }

    case class Observations(numTrials: Int, numHeads: Int) extends ObservationsBase{
    	assert(numTrials >= numHeads)
    	def proportionHeads = numHeads.asInstanceOf[Double] / numTrials
    }
    
    case class Output(simulated: Observations) extends OutputBase {
      def distanceTo(obs: Observations): Double = 
        math.abs(simulated.proportionHeads - obs.proportionHeads)
    }
    
    def init(p: Parameters, obs: Observations) = new Samplable[Output, Random] {
		override def sample(implicit r: Random) = {
			def coinToss() = r.nextBoolean(p.pHeads)
			Output(Observations(obs.numTrials, (1 to obs.numTrials).map(i => coinToss).count(identity)))
		}
    }
    
  }
  
  val model = new Model
  import model._
  
  val prior = new Prior[Parameters, Random]{
    def density(p: Parameters) = {
      if(p.pHeads > max || p.pHeads < min) 0.0
      else 1.0 / ((max - min)*(max - min))
    }
    
    def sample(implicit r: Random) = {
      Parameters(r.nextDouble(min, max))
    }
  }
  
  val obs = Observations(10,5)
  
	object Writer extends PopulationWriter{
		def apply(p: Seq[Parameters], tolerance: Double){
			new CSVTableWriter(wd.resolve(tolerance+".csv"), true).apply(
				Column(p.map(_.pHeads), "P")
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
			new ABCParameters(10, 5000, 1, 20, 500),
			new SerialRunner,
			None//Some(Writer)
	).map(_.pHeads)
	
	QuickPlot.writeDensity(wd, "script", Map("data" -> resultParams.toEmpiricalSeq))
}