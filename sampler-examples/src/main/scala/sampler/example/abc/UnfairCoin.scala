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

import java.math.MathContext
import java.nio.file.{Files, Paths}

import com.typesafe.config.ConfigFactory
import org.apache.commons.io.FileUtils
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.random.{MersenneTwister, SynchronizedRandomGenerator}
import play.api.libs.json.JsValue
import sampler.abc._
import sampler.distribution.Distribution
import sampler.io.{Tokenable, Tokens}
import sampler.maths.Random
import sampler.r.script.RScript

object UnfairCoin extends UnfairCoinCommon with App {
  ABC(CoinModel, abcConfig, abcReporting)

  StandardReport.doPlotting(wd)

//  plot()
}

object ResumeUnfairCoin extends UnfairCoinCommon with App {
  val prevGenJson =
    """
{
  "generation" : 4,
  "tolerance" : 2,
  "acceptance-ratio" : 0.25,
  "particle-summary": {
    "pHeads": [0.1, 0.2, 0.3],
    "weight": [1,1,1]
  },
  "particle-details" : [
    {
      "Comment": " p=ParameterSet, s=RepScores, w=Weight",
      "p": {"pHeads": 0.1},
      "s": [1,5,1],
      "w": 1
    },{
      "p": {"pHeads": 0.2},
      "s": [1,2,2],
      "w": 1
    },{
      "p": {"pHeads": 0.3},
      "s": [1,1,1],
      "w": 1
    }
  ]
}  
  """
  FileUtils.writeStringToFile(wd.resolve("Gen004.json").toFile, prevGenJson)

  def parser(toks: Map[String, JsValue]) = CoinParams(toks("pHeads").as[Double])

  val prevGeneration = Population.fromJson(prevGenJson, parser _)

  val sixGenConfig = ABCConfig(ConfigFactory
    .parseString("unfair-coin-example.abc.job.generations = 6")
    .withFallback(ConfigFactory.load())
    .getConfig("unfair-coin-example"))

  ABC.resume(CoinModel, sixGenConfig, prevGeneration, abcReporting)
  plot()
}

trait UnfairCoinCommon {
  val wd = Paths.get("results", "UnfairCoin")
  Files.createDirectories(wd)

  val abcConfig = ABCConfig(ConfigFactory.load.getConfig("unfair-coin-example"))
  val abcReporting = StandardReport[CoinParams](wd, abcConfig)
  def plot() {
    val rScript =
      """
      lapply(c("ggplot2", "reshape2", "jsonlite", "plyr"), require, character.only=T)
      
      load = function(file) {
      	raw = fromJSON(file)
      	data.frame(
      	  raw$`particle-summary`, 
      	  Generation=factor(raw$generation), 
      	  ToleranceFactor = factor(raw$tolerance), 
      	  AcceptanceRatio = raw$`acceptance-ratio`
      	)
      }
      merged = ldply(lapply(Sys.glob("Gen*.json"), load))
      
      meta = unique(merged[,c("Generation", "ToleranceFactor", "AcceptanceRatio")])
      meta$Tolerance = as.numeric(levels(meta$ToleranceFactor)[meta$ToleranceFactor])
      meta = subset(meta, select = c(-ToleranceFactor))
      
      isFinite = function(x) { !is.infinite(x) }
      
      sampleFromGen = function(n){
      	gen = merged[merged$Generation == n,]
      	gen[sample(nrow(gen), replace = T, 1000, prob = gen$weight),]
      }
      
      sampled = ldply(meta$Generation, sampleFromGen)
      
      pdf("generations.pdf", width=4.13, height=2.91)
      
      ggplot(melt(meta), aes(x=Generation, y=value, fill=isFinite(value))) +
      	geom_bar(stat="identity") +
        facet_grid(variable ~ ., scales="free")
      
      ggplot(merged, aes(x=pHeads, colour=Generation)) + 
      	geom_density() + 
      	scale_colour_hue(h=c(-270, 0)) +
      	scale_x_continuous(limits=c(0, 1)) +
      	ggtitle("Ignoring particle weights")
      
      ggplot(sampled, aes(x=pHeads, colour=Generation)) + 
      	geom_density(adjust = 1.3) + 
      	scale_colour_hue(h=c(-270, 0)) +
      	scale_x_continuous(limits=c(0, 1)) +
      	ggtitle("Sampling from weights")
      
      dev.off()	
    """

    RScript(rScript, wd.resolve("script.r"))
  }
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

  case class Observed(numTrials: Int, numHeads: Int) extends Serializable

  val observedData = Observed(100, 70)

  val prior = new Prior[CoinParams] {
    def density(p: CoinParams) = {
      if (p.pHeads > 1 || p.pHeads < 0) 0.0
      else 1.0
    }

    val distribution = Distribution.from { r =>
      CoinParams(r.nextDouble(0.0, 1.0))
    }
  }

  private val normal = {
    val syncRand = new SynchronizedRandomGenerator(new MersenneTwister())
    new NormalDistribution(syncRand, 0, 0.5, NormalDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
  }

  def perturb(params: CoinParams) = CoinParams(params.pHeads + normal.sample)

  def perturbDensity(a: CoinParams, b: CoinParams) = normal.density(a.pHeads - b.pHeads)

  def distanceToObservations(p: CoinParams) = Distribution.from { (random: Random) =>
    def coinToss() = random.nextBoolean(p.pHeads)

    val simulatedHeads = (1 to observedData.numTrials)
      .map(_ => coinToss)
      .count(identity)
    if (simulatedHeads == observedData.numHeads) 0
    else 1
  }
}
