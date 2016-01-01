/*
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
import sampler.abc.Population
import sampler.abc.Tokenable
import play.api.libs.json.{JsNull,Json,JsString,JsValue}
import sampler.io.Rounding
import org.apache.commons.io.FileUtils
import play.api.libs.json.Writes
import sampler.abc.Tokens
import sampler.abc.Tokenable
import play.api.libs.json.JsNumber
import java.math.MathContext

object UnfairCoin extends App with ToNamedSeq{
	val wd = Paths.get("results", "UnfairCoin")
	Files.createDirectories(wd)
	
	val abcParameters = ABCConfig.fromTypesafeConfig(ConfigFactory.load(), "unfair-coin-example")
	val abcReporting = { pop: Population[CoinParams] =>
		val json = Json.prettyPrint(pop.toJSON() )
		FileUtils.write(wd.resolve(s"Gen${pop.iteration}.json").toFile, json)
	}

	ABC(CoinModel, abcParameters, abcReporting)
	
	val rScript = """
lapply(c("ggplot2", "reshape", "jsonlite", "plyr"), require, character.only=T)

load = function(file) {
	raw = fromJSON(file)
	data.frame(Generation=factor(raw$iteration), PHeads=raw$particles$pHeads, wt=raw$particles$weight)
}
merged = ldply(lapply(Sys.glob("Gen*.json"), load))

sampleFromGen = function(n){
	gen = merged[merged$Generation == n,]
	gen[sample(nrow(gen), replace = T, 10000, prob = gen$wt),]
}
sampled = rbind(sampleFromGen(1), sampleFromGen(2), sampleFromGen(3))

pdf("generations.pdf", width=4.13, height=2.91)

ggplot(merged, aes(x=PHeads, colour=Generation)) + 
	geom_density() + 
	scale_x_continuous(limits=c(0, 1)) +
	ggtitle("Ignoring particle weights")

ggplot(sampled, aes(x=PHeads, colour=Generation)) + 
	geom_density() + 
	scale_x_continuous(limits=c(0, 1)) +
	ggtitle("Sampling from weights")

dev.off()	
"""
	RScript(rScript, wd.resolve("script.r"))
}

case class CoinParams(pHeads: Double) {
	def fieldNames = Seq("PHeads")
	def fields = Seq(pHeads.toString)
}
object CoinParams {
	implicit val tokener: Tokenable[CoinParams] = new Tokenable[CoinParams] {
		val mc = new MathContext(6)
		def getTokens(p: CoinParams) = Tokens.named(
			"pHeads" -> BigDecimal(p.pHeads, mc)
		)
	}
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
