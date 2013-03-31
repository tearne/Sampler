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
import sampler.run.cluster.Runner
import sampler.run.SerialRunner
import sampler.abc.ABCMethod
import sampler.r.QuickPlot
import sampler.abc.ABCModel
import sampler.math._
import sampler.data.Samplable
import sampler.data.Empirical._
import sampler.abc.ABCMeta
import sampler.abc.Prior
import sampler.run.SerialRunner
import sampler.io.CSVTableWriter
import sampler.data.Types._

object UnfairCoin extends UnfairCoinBase with App {
	if(args.nonEmpty) System.setProperty("akka.remote.netty.port", args(0))
	//else System.setProperty("akka.remote.netty.port", "2555")
	
	val abcMethod = new ABCMethod(CoinModel)
}

//object TestUnfairCoin extends UnfairCoinBase with App {
//  val abcMethod = new ABCMethod(TestCoinModel)
//}

trait UnfairCoinBase {
//	val runner = new Runner
	val runner = new SerialRunner
	
	val abcMethod: ABCMethod[CoinModel.type]
	implicit val abcRandomSource = CoinModel.abcRandomSource

	val population0 = abcMethod.init
	
	//TODO Fix slightly nasty mapping to population values
	val finalPopulation = abcMethod.run(population0, runner).map(_.map(_.value))
//	runner.shutdown

  //TODO Make Column use Seq to avoid need for random source
	val headsDensity = finalPopulation.get.map(_.pHeads).toEmpiricalSeq
	
	val wd = Paths.get("egout", "coinTossABC")
	Files.createDirectories(wd)
	new CSVTableWriter(wd.resolve("results.csv"), true)(
		Column(headsDensity.values, "TruePos")
	)
	
	//QuickPlot.writeDensity(wd, "posterior", Map("data" -> headsDensity))
}

object CoinModel extends CoinModelBase {
  val abcRandomSource = new RandomSourceImpl {} // Used implicitly ... check use sites
  val coinModelRandomSource = new RandomSourceImpl {}    // Used explicitly ... check use sites
}

//object TestCoinModel extends CoinModelBase {
//  val abcRandomSource = new RandomSource {} // Used implicitly ... check use sites
//  val coinModelRandomSource = new RandomSource {
//    def newRandome = new Random {
//      // etc ...
//    }
//  }    // Used explicitly ... check use sites
//}

trait CoinModelBase extends ABCModel with Serializable{
  val statistics = new StatisticsComponentImpl with Serializable{}

  implicit val abcRandomSource: RandomSource 
  val abcRandom = abcRandomSource.newRandom

  val coinModelRandomSource: RandomSource

	val observations = Observations(10,7)
    val meta = new ABCMeta(
    	reps = 100000,
		numParticles = 1000, 
		refinements = 10,
		particleRetries = 100, 
		particleChunking = 10
	)
	
	implicit def toProbability(d: Double) = Probability(d)
	
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
      val r = coinModelRandomSource.newRandom
      override def sample() = {
        def coinToss() = r.nextBoolean(p.pHeads)
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
