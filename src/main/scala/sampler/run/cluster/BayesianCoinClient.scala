package sampler.run.cluster

import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.pattern.ask
import sampler.fit.ABCModel
import sampler.math.Random
import sampler.data.Samplable
import sampler.math.Probability
import sampler.fit.Prior
import sampler.run.AbortableRunner
import akka.util.Timeout
import sampler.run.AbortFunction
import sampler.run.AbortableJob
import scala.concurrent.Future
import scala.concurrent.Await
import sampler.fit.ABCComponent
import sampler.data.SampleBuilderComponent
import sampler.data.SerialSampleBuilder
import sampler.fit.ABCParameters
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

	val myModel = new CoinModel
	//import myModel._
	
	object ABCRunner extends ABCComponent with SampleBuilderComponent{
		val builder = SerialSampleBuilder
	}
	val random = new Random()
	
	val resultParams = ABCRunner(
			myModel,
			new ClusterRunner,
			random
	).map(_.pHeads)
	
//	val wd = Paths.get("examples").resolve("coinTossABC")
//	Files.createDirectories(wd)
//	QuickPlot.writeDensity(wd, "script", Map("data" -> resultParams.toEmpiricalSeq))
}

class ClusterRunner{
	import scala.concurrent.duration._
	
	val system = ActorSystem("MasterSystem")
	implicit val timeout = Timeout(1.minutes)
	
	val master = system.actorOf(Props[Master], name = "master")
	import system.dispatcher
	
	def apply[T, R <: Random](jobs: Seq[Job[T,R]]): Seq[T] = {
		val futures = jobs.map(master ? _)
		
		//TODO Nasty!
		val fSeq = Future.sequence(futures).asInstanceOf[Future[Seq[T]]]
		
		val res = Await.result(fSeq, timeout.duration)
		res
	}
	
	def shutdown() {
		system.shutdown
	}
}

class CoinModel extends ABCModel[Random] with SampleBuilderComponent with Serializable{
	val builder = SerialSampleBuilder
	//implicit val random = new Random()
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
    
    def initModel(p: Parameters, obs: Observations) = new Samplable[Output, Random] with Serializable{
		override def sample(implicit r: Random) = {
			def coinToss() = r.nextBoolean(p.pHeads)
			Output(Observations(obs.numTrials, (1 to obs.numTrials).map(i => coinToss).count(identity)))
		}
    }
    val observations = Observations(10,5)
    val abcParameters = new ABCParameters(10, 10, 1, 1, 500)
    val prior = new Prior[Parameters, Random] with Serializable{
	    def density(p: Parameters) = {
	      if(p.pHeads > 1 || p.pHeads < 0) 0.0
	      else 1.0
	    }
	    
	    def sample(implicit r: Random) = Parameters(r.nextDouble(0.0, 1.0))
    }
  }