/*
 * Copyright (c) 2012-2013 Crown Copyright 
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

import java.nio.file.Files
import java.nio.file.Paths
import sampler.abc.ABCParameters
import sampler.abc.ABCMethod
import sampler.abc.ABCModel
import sampler.abc.Prior
import sampler.data.Empirical.RichIndexedSeq
import sampler.data.Samplable
import sampler.data.Types.Column
import sampler.data.Types.DoubleColumn
import sampler.io.CSVTableWriter
import sampler.math.Probability
import sampler.math.Random
import sampler.math.StatisticsComponent
import sampler.r.QuickPlot
import sampler.run.actor.NodeApplication
import sampler.run.actor.PortFallbackSystem
import sampler.abc.population.ActorPopulationExecutor
import sampler.abc.population.ActorPopulationDispatcher
import sampler.abc.population.LocalPopulationBuilder
import sampler.abc.population.PopulationBuilder

object UnfairCoinApplication extends App 
	with UnfairCoinFactory 
	with UnfairCoin
	
object UnfairCoinWorker extends App {
	new NodeApplication(new ActorPopulationExecutor(CoinModel, Random))
}

trait UnfairCoinFactory{
	val meta = new ABCParameters(
    	reps = 10000,
		numParticles = 100, 
		refinements = 10,
		particleRetries = 100, 
		particleChunking = 50
	)
	val abcMethod = new ABCMethod(CoinModel, meta, Random)
	
	val pBuilder = ActorPopulationDispatcher(PortFallbackSystem("ClusterSystem"))
//	val pBuilder = LocalPopulationBuilder()
}

trait UnfairCoin {
	val pBuilder: PopulationBuilder
	
	implicit val abcMethod: ABCMethod[CoinModel.type]

	val population0 = abcMethod.init
	
	//TODO Fix slightly nasty mapping to population values
	val finalPopulation = abcMethod.run(population0, pBuilder).map(_.map(_.value))

	val headsDensity = finalPopulation.get.map(_.pHeads)
	
	val wd = Paths.get("egout", "UnfairCoin")
	Files.createDirectories(wd)
	new CSVTableWriter(wd.resolve("results.csv"), true)(
		Column(headsDensity, "TruePos")
	)
	
	implicit val r = Random
	QuickPlot.writeDensity(wd, "posterior", Map("data" -> headsDensity))
}

object CoinModel extends CoinModelBase {
  val abcRandom = Random
  val modelRandom = Random
  val statistics = StatisticsComponent
}

trait CoinModelBase extends ABCModel with Serializable{
	implicit val abcRandom: Random
  	val modelRandom: Random

	val observations = Observations(10,7)
	
    case class Parameters(pHeads: Double) extends ParametersBase with Serializable{
      lazy val kernel = Samplable.normal(0,0.5)
      
      def perturb() = Parameters(pHeads + kernel.sample())
      def perturbDensity(that: Parameters) = kernel.density(pHeads - that.pHeads)
    }

    case class Observations(numTrials: Int, numHeads: Int) extends ObservationsBase with Serializable{
    	assert(numTrials >= numHeads)
    	def proportionHeads = numHeads.asInstanceOf[Double] / numTrials
    }
    
    case class Output(simulated: Observations) extends OutputBase with Serializable{
      def distanceTo(obs: Observations): Double = {
    	assert(simulated.numTrials == obs.numTrials)
      	math.abs(simulated.numHeads - obs.numHeads)
      }
    }
    
    def samplableModel(p: Parameters, obs: Observations) = new Samplable[Output] with Serializable{
      val r = modelRandom
      override def sample() = {
        def coinToss() = r.nextBoolean(Probability(p.pHeads))
        Output(Observations(obs.numTrials, (1 to obs.numTrials).map(i => coinToss).count(identity)))
      }
    }
    
    val prior = new Prior[Parameters] with Serializable{
	    def density(p: Parameters) = {
	      if(p.pHeads > 1 || p.pHeads < 0) 0.0
	      else 1.0
	    }
	    
	    def sample() = Parameters(abcRandom.nextDouble(0.0, 1.0))
    }
}
