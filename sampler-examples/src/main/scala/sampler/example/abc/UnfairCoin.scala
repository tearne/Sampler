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

package sampler.example.abc

import java.nio.file.Files
import java.nio.file.Paths
import org.apache.commons.math3.distribution.NormalDistribution
import sampler.Implicits.RichFractionalSeq
import sampler.abc.ABCModel
import sampler.abc.ABCParameters
import sampler.abc.Prior
import sampler.data.Distribution
import sampler.math.Probability
import sampler.math.Random
import sampler.r.QuickPlot.writeDensity
import sampler.abc.builder.local.LocalPopulationBuilder
import sampler.abc.ABCMethod
import sampler.cluster.abc.Node
import sampler.cluster.abc.population.ClusterParticleBuilder
import sampler.cluster.abc.population.ClusteringPopulationBuilder
import sampler.cluster.actor.PortFallbackSystem
import sampler.abc.builder.PopulationBuilder

object UnfairCoinApplication extends App 
	with UnfairCoinFactory 
	with UnfairCoin

	
object UnfairCoinWorker extends App {
	new Node(ClusterParticleBuilder(CoinModel, Random))
}

trait UnfairCoinFactory{
	val meta = new ABCParameters(
    	reps = 100,
		numParticles = 100, 
		refinements = 50,
		particleRetries = 100, 
		particleChunking = 100
	)
	val pBuilder = ClusteringPopulationBuilder(PortFallbackSystem("ClusterSystem"))
//	val pBuilder = LocalBuilder()
}

trait UnfairCoin {
	val pBuilder: PopulationBuilder
	val meta: ABCParameters
	
	val finalPopulation = ABCMethod(
			CoinModel, 
			meta, 
			pBuilder, 
			Random
	).get

	val headsDensity = finalPopulation.map(_.pHeads)
	
	val wd = Paths.get("results", "UnfairCoin")
	Files.createDirectories(wd)
	
	writeDensity(
		wd, 
		"posterior", 
		headsDensity.continuous("P[Heads]")
	)
}

object CoinModel extends CoinModelBase{
  val abcRandom = Random
  val modelRandom = Random
}

trait CoinModelBase extends ABCModel with Serializable{
	implicit val abcRandom: Random
  	val modelRandom: Random

	val observed = Observed(10,7)
	
    case class Parameters(pHeads: Double) extends ParametersBase with Serializable{
	  val normal = new NormalDistribution(0,0.5)
      def perturb() = Parameters(pHeads + normal.sample)
      def perturbDensity(that: Parameters) = normal.density(pHeads - that.pHeads)
    }

    case class Observed(numTrials: Int, numHeads: Int) extends Serializable{
    	assert(numTrials >= numHeads)
    	def proportionHeads = numHeads.asInstanceOf[Double] / numTrials
    }
    
    case class Simulated(res: Observed) extends SimulatedBase with Serializable{
      def distanceToObserved: Double = {
    	assert(res.numTrials == observed.numTrials)
    	val t = math.abs(res.numHeads - observed.numHeads)
    	//println("fit is "+t+" raw is "+res)
      	t.toDouble
      }
    }
    
    def modelDistribution(p: Parameters) = new Distribution[Simulated] with Serializable{
      val r = modelRandom
      override def sample() = {
        def coinToss() = r.nextBoolean(Probability(p.pHeads))
        Simulated(Observed(
        		observed.numTrials, 
        		(1 to observed.numTrials)
        			.map(i => coinToss)
        			.count(identity)
        ))
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
