/*
 * Copyright (c) 2012-2015 Crown Copyright
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
import java.nio.file.StandardOpenOption.{APPEND,CREATE}
import org.apache.commons.math3.distribution.NormalDistribution
import com.typesafe.config.ConfigFactory
import sampler.abc.ABC
import sampler.abc.Model
import sampler.abc.Prior
import sampler.abc.actor.message.Report
import sampler.abc.config.ABCConfig
import sampler.data.Distribution
import sampler.math.Random
import sampler.io.CSV
import sampler.io.CSV
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import org.apache.commons.math3.random.MersenneTwister
import sampler.r.script.ToNamedSeq
import sampler.r.script.QuickPlot
import sampler.r.script.RScript

object UnfairCoin extends App with ToNamedSeq{
	/*
	 * ABCParameters loaded from application.conf
	 */
	val wd = Paths.get("results", "UnfairCoin")
//	Files.createDirectories(wd.getParent)
	Files.createDirectories(wd)
	
	
	val tempCSV = Files.createTempFile(null, null)
	tempCSV.toFile().deleteOnExit
	
	val abcParameters = ABCConfig.fromTypesafeConfig(ConfigFactory.load(), "unfair-coin-example")
	val abcReporting = { report: Report[CoinParams] =>
		val lineToks = s"Gen${report.generationId}" +: report.posterior.map(_.pHeads)
		CSV.writeLine(
				tempCSV, 
				lineToks,
				APPEND, CREATE
		)
	}
	
	val finalGeneration = ABC(CoinModel, abcParameters, abcReporting).map(_.pHeads)
	
	// Make plot of final generation (posterior)
	QuickPlot.writeDensity(
		wd.resolve("posterior.pdf"),
		"9.00", "3.00",
		finalGeneration.continuous("P[Heads]")
	)
	
	
	// Make plot showing all generations
	CSV.transpose(tempCSV, wd.resolve("output.csv"))
	
	val rScript = s"""
lapply(c("ggplot2", "reshape"), require, character.only=T)
data = read.csv("output.csv")
pdf("generations.pdf", width=4.13, height=2.91) #A7 landscape paper
ggplot(melt(data), aes(x=value, colour=variable)) + geom_density() + scale_x_continuous(limits=c(0, 1))
dev.off()	
"""
	RScript(rScript, wd.resolve("script.r"))
}

case class CoinParams(pHeads: Double){
	def fieldNames = Seq("PHeads")
	def fields = Seq(pHeads.toString)
}

object CoinModel extends Model[CoinParams] {
  	val random = Random

	case class Observed(numTrials: Int, numHeads: Int) extends Serializable
  	val observedData = Observed(100,70)

	val prior = new Prior[CoinParams] with Serializable{
	    def density(p: CoinParams) = {
	      if(p.pHeads > 1 || p.pHeads < 0) 0.0
	      else 1.0
	    }
	    
	    def draw() = {
	    	CoinParams(random.nextDouble(0.0, 1.0))
	    }
    }
  	
    private val normal = {
    	val syncRand = new SynchronizedRandomGenerator(new MersenneTwister())
		new NormalDistribution(syncRand, 0, 0.5, NormalDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
    }
	def perturb(params: CoinParams) = CoinParams(params.pHeads + normal.sample)
  	def perturbDensity(a: CoinParams, b: CoinParams) = normal.density(a.pHeads - b.pHeads)
    
    def distanceToObservations(p: CoinParams) = new Distribution[Double] with Serializable{
    	override def sample() = {
    		def coinToss() = random.nextBoolean(p.pHeads)
    		val simulatedHeads = (1 to observedData.numTrials)
    			.map(i => coinToss)
    			.count(identity)
    		if(simulatedHeads == observedData.numHeads) 0
    		else 1
    	}
    }
}
