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

import java.nio.file.{Paths, Files}
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
import sampler.math.Random
import sampler.run.Aborter
import sampler.run.akka.AkkaUtil
import sampler.run.ActorJobRunner
import sampler.run.akka.client.FailFastRunner
import sampler.run.akka.worker.NodeApp
import sampler.run.akka.worker.RunnerFactory
import sampler.run.akka.client.Runner
import sampler.data.Empirical
import sampler.run.WrappedAborter
import sampler.abc.ABCUtil._
import sampler.abc.ABCUtil

object UnfairCoinApp extends App 
	with UnfairCoinFactory 
	with UnfairCoin
	
object UnfairCoinRemoteWorker extends App{
	ABCUtil.startWorkerNode(CoinModel)
}

trait UnfairCoinFactory{
	val abcMethod = new ABCMethod(CoinModel)
//	val system = AkkaUtil.systemWithPortFallback("ClusterSystem")
	
//	val tasker = new ABCUtil.ActorTasker(new FailFastRunner(system))
	val tasker = new ABCUtil.LocalTasker(SerialRunner())
}

trait UnfairCoin {
	val tasker: ABCUtil.Tasker
	
	val abcMethod: ABCMethod[CoinModel.type]
	implicit val abcRandom = CoinModel.abcRandom

	val population0 = abcMethod.init
	
	//TODO Fix slightly nasty mapping to population values
	val finalPopulation = abcMethod.run(population0, tasker).map(_.map(_.value))

	val headsDensity = finalPopulation.get.map(_.pHeads)
	
	val wd = Paths.get("egout", "UnfairCoin")
	Files.createDirectories(wd)
	new CSVTableWriter(wd.resolve("results.csv"), true)(
		Column(headsDensity, "TruePos")
	)
	
	QuickPlot.writeDensity(wd, "posterior", Map("data" -> headsDensity.toEmpiricalSeq))
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
    val meta = new ABCMeta(
    	reps = 10000,
		numParticles = 100, 
		refinements = 10,
		particleRetries = 100, 
		particleChunking = 50
	)
	
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
