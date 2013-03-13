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

import java.nio.file.{Paths, Files}
import sampler.run.ClusterRunner
import sampler.run.SerialRunner
import sampler.abc.ABCMethod
import sampler.r.QuickPlot
import sampler.abc.ABCModel
import sampler.math.Random
import sampler.math.Probability
import sampler.math.StatisticsComponentImpl
import sampler.data.Samplable
import sampler.data.Empirical._
import sampler.abc.ABCMeta
import sampler.abc.Prior
import sampler.run.SerialRunner

object UnfairCoin extends App{
	if(args.nonEmpty) System.setProperty("akka.remote.netty.port", args(0))
	else System.setProperty("akka.remote.netty.port", "2555")
	
	//val runner = new ClusterRunner
	val runner = new SerialRunner
	
	val abcMethod = new ABCMethod(CoinModel)

	val population0 = abcMethod.init
  // FIXME: get could fail, preventing runner shutdown
	val finalPopulation = abcMethod.run(population0, runner).get.map(_.value.pHeads)
	
	//runner.shutdown
	
	val wd = Paths.get("egoutput", "coinTossABC")
	Files.createDirectories(wd)
	QuickPlot.writeDensity(wd, "script", Map("data" -> finalPopulation.toEmpiricalSeq))
}

object CoinModel extends ABCModel with Serializable{
  val statistics = new StatisticsComponentImpl {}
  type R = Random
  val random = new Random()
	val observations = Observations(10,5)
    val meta = new ABCMeta(
    	reps = 10,
		numParticles = 100, 
		tolerance = 1, 
		refinements = 6,
		particleRetries = 100, 
		particleChunking = 100
	)
	
	implicit def toProbability(d: Double) = Probability(d)
	
    case class Parameters(pHeads: Double) extends ParametersBase with Serializable{
      val kernel = Samplable.normal(0,1)
      
      def perturb() = Parameters(pHeads + kernel.sample())
      def perturbDensity(that: Parameters) = kernel.density(pHeads - that.pHeads)
    }

    case class Observations(numTrials: Int, numHeads: Int) extends ObservationsBase with Serializable{
    	assert(numTrials >= numHeads)
    	def proportionHeads = numHeads.asInstanceOf[Double] / numTrials
    }
    
    case class Output(simulated: Observations) extends OutputBase with Serializable{
      def distanceTo(obs: Observations): Double = 
        math.abs(simulated.proportionHeads - obs.proportionHeads)
    }
    
    def samplableModel(p: Parameters, obs: Observations) = new Samplable[Output] with Serializable{
      val r = new Random()
      override def sample() = {
        def coinToss() = r.nextBoolean(p.pHeads)
        Output(Observations(obs.numTrials, (1 to obs.numTrials).map(i => coinToss).count(identity)))
      }
    }
    
    val prior = new Prior[Parameters] with Serializable{
      //val r = new Random()
	    def density(p: Parameters) = {
	      if(p.pHeads > 1 || p.pHeads < 0) 0.0
	      else 1.0
	    }
	    
	    def sample() = Parameters(random.nextDouble(0.0, 1.0))
    }
}
