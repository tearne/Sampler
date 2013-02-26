package sampler.run.cluster

import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.pattern.ask
import sampler.math.Random
import sampler.data.Samplable
import sampler.math.Probability
import sampler.run.AbortableRunner
import akka.util.Timeout
import sampler.run.AbortFunction
import sampler.run.AbortableJob
import scala.concurrent.Future
import scala.concurrent.Await
//import sampler.fit.ABCComponent
import sampler.data.SampleBuilderComponent
import sampler.data.SerialSampleBuilder
import sampler.r.QuickPlot
import java.nio.file.Paths
import java.nio.file.Files
import sampler.data.Empirical._

object BayesianCoinClientApp extends App{
	if(args.nonEmpty) System.setProperty("akka.remote.netty.port", args(0))
	else System.setProperty("akka.remote.netty.port", "2555")
	
//	System.setProperty("akka.loglevel", "WARNING")
//	val system = ActorSystem("ClusterSystem")
//	system.actorOf(Props[TestClientActor], name = "testClient")

//	val myModel = new CoinModelImpl{
//		
//	}

	//object CoinModelImpl extends CoinModel 
	
	//import myModel._
//	
//	object ABCRunner extends ABCComponent{
//		val builder = SerialSampleBuilder
//	}
	val random = new Random()
	
	val runner = new ClusterRunner
	
	val encapPopulation0 = ABCBase.init(CoinModel)
	val finalEncapPopulation = ABCBase.evolve(encapPopulation0, runner).get//.population
	val finalPopulation = finalEncapPopulation.population.map(_.value.asInstanceOf[CoinModel.Parameters].pHeads)
	
	runner.shutdown
	
	val wd = Paths.get("examples").resolve("coinTossABC")
	Files.createDirectories(wd)
	QuickPlot.writeDensity(wd, "script", Map("data" -> finalPopulation.toEmpiricalSeq))
}

class ClusterRunner{
	import scala.concurrent.duration._
	
	val system = ActorSystem("ClusterSystem")
	implicit val timeout = Timeout(10.minutes)
	
	val master = system.actorOf(Props[Master], name = "master")
	import system.dispatcher
	
	def apply[T, R <: Random](jobs: Seq[Job[T]]): Seq[Option[EncapsulatedPopulation[R]]] = {
		val futures = jobs.map(master ? _)
		
		val fSeq = Future.sequence(futures).mapTo[Seq[Option[EncapsulatedPopulation[R]]]]
		
		val res = Await.result(fSeq, timeout.duration)
		res
	}
	
	def shutdown() {
		system.shutdown
	}
}

object CoinModel extends ABCModel[Random] with SampleBuilderComponent with Serializable{
	val builder = SerialSampleBuilder
	val random = new Random()
	implicit def toProbability(d: Double) = Probability(d)
	
    case class Parameters(pHeads: Double) extends ParametersBase with Serializable{
      val kernel = Samplable.normal(0,1)
      
      def perturb(random: Random) = Parameters(pHeads + kernel.sample(random))
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
    
    def samplableModel(p: Parameters, obs: Observations) = new Samplable[Output, Random] with Serializable{
		override def sample(implicit r: Random) = {
			def coinToss() = r.nextBoolean(p.pHeads)
			Output(Observations(obs.numTrials, (1 to obs.numTrials).map(i => coinToss).count(identity)))
		}
    }
    val observations = Observations(10,5)
    val meta = new ABCMeta(
    	reps = 10, 
		numParticles = 350, 
		tolerance = 1, 
		refinements = 6, 
		particleRetries = 100, 
		particleChunking = 10
	)
    val prior = new Prior[Parameters, Random] with Serializable{
	    def density(p: Parameters) = {
	      if(p.pHeads > 1 || p.pHeads < 0) 0.0
	      else 1.0
	    }
	    
	    def sample(implicit random: Random) = Parameters(random.nextDouble(0.0, 1.0))
    }
  }