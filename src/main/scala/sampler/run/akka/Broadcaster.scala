package sampler.run.akka

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.cluster.Cluster
import akka.actor.ActorRef
import akka.cluster.ClusterEvent._
import akka.cluster.Member
import akka.actor.RootActorPath
import akka.cluster.MemberStatus

case class Broadcast(msg: Any)

class Broadcaster extends Actor with ActorLogging{
	val cluster = Cluster(context.system)
  	override def preStart(): Unit = cluster.subscribe(self, classOf[ClusterDomainEvent])
	override def postStop(): Unit = cluster.unsubscribe(self)
	
	val workers = collection.mutable.Set.empty[ActorRef]
	val workerPath = Seq("user", "worker")
	
	def attemptWorkerHandshake(m: Member){
		val workerCandidate = context.actorFor(RootActorPath(m.address) / workerPath)
		workerCandidate ! StatusRequest
		log.debug("Attempting handshake with potential worker {}", workerCandidate)
	}
	
	def receive = {
		case state: CurrentClusterState => 
  		  	state.members.filter(_.status == MemberStatus.Up).foreach(attemptWorkerHandshake)
  		case MemberUp(m) => 
  		  	log.info("Member {} is up", m)
  		  	attemptWorkerHandshake(m)
  		case MemberRemoved(m) =>
  		  	val potentialWorker = context.actorFor(RootActorPath(m.address) / workerPath)
  		  	log.info("Downref = "+potentialWorker.path)
  		  	if(workers.contains(potentialWorker)) workers -= potentialWorker
  		  	log.info("{} workers in pool", workers.size)
  		  	
  		case WorkerBusy =>
  		  	workers += sender 
  		  	log.info("{} workers in pool", workers.size)
  		case WorkerIdle =>
  			workers += sender
  			log.info("{} workers in pool", workers.size)
  			context.parent.forward(WorkerIdle)
		
  		case Broadcast(msg) => 
			log.info("Broadcasting {} on behalf of {}", msg, sender)
			workers.foreach(_.forward(msg))
	}
}