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
import sampler.Implicits._
import sampler.data.Distribution
import sampler.math.Probability
import sampler.math.Random
import sampler.r.QuickPlot.writeDensity
import com.typesafe.config.ConfigFactory
import sampler.cluster.abc.Model
import sampler.cluster.abc.Prior
import sampler.cluster.abc.ABC
import sampler.cluster.abc.config.ABCConfig

object ClusteredUnfairCoin extends App {
	/*
	 * ABCParameters loaded from application.conf
	 */
	val parameters = ABCConfig.fromConfig(ConfigFactory.load(), "unfair-coin-example")
	
	val headsDensity = ABC(CoinModel, parameters).map(_.pHeads)
	
	val wd = Paths.get("results", "UnfairCoin")
	Files.createDirectories(wd)
	
	writeDensity(
		wd, 
		"posterior", 
		headsDensity.continuous("P[Heads]")
	)
}

case class CoinParams(pHeads: Double) extends Serializable

object CoinModel extends Model[CoinParams] {
  	val random = Random

  	val observed = Observed(10,7)

	val prior = new Prior[CoinParams] with Serializable{
	    def density(p: CoinParams) = {
	      if(p.pHeads > 1 || p.pHeads < 0) 0.0
	      else 1.0
	    }
	    
	    def sample() = CoinParams(random.nextDouble(0.0, 1.0))
    }
	
    case class Observed(numTrials: Int, numHeads: Int) extends Serializable
    
    private val normal = new NormalDistribution(0,0.5)
	def perturb(params: CoinParams) = CoinParams(params.pHeads + normal.sample)
  	def perturbDensity(a: CoinParams, b: CoinParams) = normal.density(a.pHeads - b.pHeads)
    
    def distanceToObservations(p: CoinParams) = new Distribution[Double] with Serializable{
    	override def sample() = {
    		def coinToss() = random.nextBoolean(Probability(p.pHeads))
    		val simulatedHeads = (1 to observed.numTrials)
    			.map(i => coinToss)
    			.count(identity)
    		math.abs(simulatedHeads - observed.numHeads)
    	}
    }
}
