//package sampler.cluster.abc.actor
//
//import akka.actor.ActorLogging
//import akka.actor.Actor
//import sampler.cluster.abc.algorithm.Weigher
//import sampler.cluster.abc.Scored
//import akka.actor.FSM
//import scala.concurrent.Future
//
//sealed trait State
//case object Idle extends State
//case object Busy extends State
//
//sealed trait Data
//case object NoQueue extends Data
//case class Waiting[P](q: Seq[Tagged[Scored[P]]]) extends Data
//
//class WeigherActor[P](weigher: Weigher[P]) extends FSM[State, Data] with Actor with ActorLogging {
//	
//	startWith(Idle, NoQueue)
//	
//	implicit val executionContext = context.system.dispatchers.lookup("sampler.work-dispatcher")
//	
//	when(Idle) {
//		case Event(job: TaggedScoredSeq[P], NoQueue) =>
//			Future{
//				weigher.weighAndFilter(scoredSeq, previousParamsWithWeights, tolerance)
//			}
//	}
//}