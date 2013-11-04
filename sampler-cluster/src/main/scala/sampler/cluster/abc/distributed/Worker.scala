package sampler.cluster.abc.distributed

import akka.actor.Actor
import sampler.abc.ABCModel
import scala.language.existentials
import sampler.cluster.abc.slave.ParticleGenerator
import java.rmi.UnexpectedException
import akka.actor.ActorLogging
import akka.actor.Props
import scala.concurrent.Future
import sampler.cluster.abc.ABCJob
import scala.util.Failure
import sampler.run.DetectedAbortionException
import scala.util.Success

case class StartGeneratingFrom(population: Set[ScoredParam], tolernace: Double)
case class Abort()
case class Aborted()


class Worker(modelRunner: ModelRunner) extends Actor with ActorLogging{
	import context._
	
	def receive = idle

	def working(currentJob: ABCJob[_]): Receive = {
		case newJob: ABCJob[_] =>
			log.info("New job recieved")
			modelRunner.abort
			become(waitingForAbortComfirmation(Some(newJob)))
			log.info("Abort signal sent, waiting for confirmation")
		case Abort =>
			modelRunner.abort
			become(waitingForAbortComfirmation(None))
		case Success(result: Seq[ScoredParam]) =>
			parent ! NewScoredParameters(result)
			startWork(currentJob)
			log.info("Result sent to parent, started another job")
		case Failure(exception) => throw exception
		case Aborted => 
			become(idle)
		case msg => 
			throw new UnsupportedOperationException("Unexpected message "+msg)
	}
	
	def waitingForAbortComfirmation(nextJob: Option[ABCJob[_]]): Receive = {
		case newJob: ABCJob[_] =>
			log.warning("Overiding previous work request (still waiting for last job to abort)")
			become(waitingForAbortComfirmation(Some(newJob)))
		case out: NewScoredParameters => 
			log.info("Ignoring result, waiting for abort comfirmation")
		case Aborted => 
			log.info("Aborted signal recieved")
			modelRunner.reset
			nextJob match{
				case Some(job) =>
					log.info("Moving to next job")
					become(working(job))
					startWork(job)
				case None =>
					log.info("Becoming idle")
					become(idle)
			}
	}
	
	def idle(): Receive = {
		case job: ABCJob[_] =>
			startWork(job)
			become(working(job))
		case msg: Aborted => 
			throw new UnexpectedException("Not expected in idle state: "+msg)
	}
	
	def startWork(job: ABCJob[_]){
		log.info("starting work")
		val me = self
		Future{
			val result = modelRunner.run(job) 
			result match{
				case Failure(e: DetectedAbortionException) =>
					log.info("Aborted future finished")
					self ! Aborted
				case anythingElse =>
					me ! anythingElse
//				case Success(result: Seq[ScoredParam]) =>	//TODO what to do about the warning
////					log.info(s"Future returned result consisteing of ${result.size} parameters")
//					me ! NewScoredParameters(result)
			}
		}
	}
}