package sampler.cluster.two

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import scala.collection._
import scala.concurrent.duration._
import akka.cluster.ClusterEvent._
import akka.cluster.MemberStatus
import akka.actor.Address
import akka.actor.RootActorPath
import akka.cluster.Member
import akka.pattern.pipe
import scala.concurrent.Future
import akka.cluster.Cluster
import akka.actor.ActorSystem
import akka.actor.Props

case class DoesWorkerExist()
case class WorkerExists()	

case class Job(f: () => Any){def apply() = f()}		//Job from a client

case class WorkerIdle()				//Worker requesting work
case class JobID(requestor: ActorRef, allocId: Int)
case class Work(job: Job, jid: JobID)
case class WorkDone(work: Work, result: Any)
case class WorkConfirmed(work: Work)
case class WorkRejected(work: Work)

case class IsWorkAvailable()
case class WorkIsAvailable()

	//
	// TODO only allow up to num CPU workers per physical node
	//

object WorkerApp extends App{
    if(args.nonEmpty) System.setProperty("akka.remote.netty.port", args(0))
	val system = ActorSystem("ClusterSystem")
	system.actorOf(Props[Worker], name = "worker")
}

object ClientApp extends App{
	val system = ActorSystem("ClusterSystem")
	val master = system.actorOf(Props[Master], name = "master")

	import scala.concurrent.Await
	import akka.pattern.ask
	implicit val askTimeout = akka.util.Timeout(1 minutes)
	implicit val ec = system.dispatcher
	
	val futures = (1 to 5).map{i => 
		master ? Job(() => {Thread.sleep(5000); i.toString})
	}
	val f = Future.sequence(futures) mapTo manifest[IndexedSeq[String]]
	
	val res = Await.result(f, askTimeout.duration)
	println(res)
}

class Worker extends Actor with ActorLogging{
	import context.dispatcher	
	case class DoneWorking()
	
	val masters = collection.mutable.Set.empty[ActorRef]
	
	val cluster = Cluster(context.system)
	override def preStart(): Unit = cluster.subscribe(self, classOf[UnreachableMember])
	override def postStop(): Unit = cluster.unsubscribe(self)
	
	def receive = idle
	
	def common: Receive = {
	  	case DoesWorkerExist => 
	  	  	sender ! WorkerExists
	  	  	if(!masters.contains(sender)) masters += sender
	  	  	log.info("Confirmed I exist to {}", sender)
	  	case UnreachableMember(m) => 
	  		val addr = m.address
	  		masters.find(_.path.address == addr).foreach{ master =>
	  			masters -= master
	  			log.info("Removed master {}", master)
	  		}
	}
	
	def idle: Receive = common orElse {
		case WorkIsAvailable =>
			if(!masters.contains(sender)) masters += sender
			sender ! WorkerIdle
			log.info("Requested work from {}", sender)
		case w: Work => 
			val master = sender
			Future{
			  	master ! WorkDone(w, w.job())
			  	DoneWorking
			}.pipeTo(self)	//Can't this be in the future?
		  	context.become(busy)
		  	sender ! WorkConfirmed(w)
		  	log.info("Confirmed start of work {} to master {}", w, sender)
	}
	
	def busy: Receive = common orElse {
		case WorkIsAvailable => 
	  	  	log.info("Ignoring WorkAvailable since busy")
	  	  	if(!masters.contains(sender)) masters += sender
		case w: Work => 
			log.info("Rejecting Work since busy now")
	  	  	sender ! WorkRejected(w)
		case DoneWorking => 
		  	masters.foreach(_ ! IsWorkAvailable) //Looking for fastest response
			context.become(idle)
	}
}

class Master extends Actor with ActorLogging{
	case class ReviewConfirmationState(work: Work, worker: ActorRef)
	
	sealed trait WorkState
	case class PendingConfirmation(work: Work) extends WorkState
	case class Reallocated() extends WorkState
	case class InProgress(worker: ActorRef, work: Work) extends WorkState
  
	val cluster = Cluster(context.system)
	override def preStart(): Unit = cluster.subscribe(self, classOf[ClusterDomainEvent])
	override def postStop(): Unit = cluster.unsubscribe(self)
	
	val workers = mutable.Set.empty[ActorRef]
	
	val workQ = mutable.Queue.empty[Work]
	//We have to do a dance with JodIDs since can't expect Jobs to pass equality checks
	var workStates = Map.empty[JobID, WorkState] 
	//val workInProgress = mutable.Map.empty[JobID, (Job, ActorRef)]
	
	val confirmTimeout = 1 seconds
	val workerPath = Seq("user", "worker")
	var nextJobId = 0
	var lastWorkAllocated: Work = _
	
	import context._
  
	def attemptHandshake(m: Member){
	  val workerCandidate = context.actorFor(RootActorPath(m.address) / workerPath)
	  workerCandidate ! DoesWorkerExist
	  log.info("Attempting handshake with potential worker {}", workerCandidate)
	}
	
	def receive = {
		//Cluster state and handshake handling
  	  	case state: CurrentClusterState => 
  		  	state.members.filter(_.status == MemberStatus.Up).foreach(attemptHandshake)
  		case MemberUp(m) => 
  		  	log.info("Member {} is up", m)
  		  	attemptHandshake(m)
  		case WorkerExists =>
  		  	workers += sender
  		  	if(!workQ.isEmpty) sender ! WorkIsAvailable
  		case UnreachableMember(m) => //Is this best thing to listen to?
  		  	val potentialWorker = context.actorFor(RootActorPath(m.address) / workerPath)
  		  	log.info("Downref = "+potentialWorker.path)
  		  	val lostWork = for(InProgress(worker, work) <- workStates.values) yield work
  		  	lostWork.foreach{work =>
  		  	  self ! work.job
  		  	  log.info("Reallocated {} from unreachable worker {}", work, potentialWorker)  		  	  
  		  	}
  		  	workStates --= lostWork.map(_.jid)
  		//Work Handling
  		case IsWorkAvailable => sender ! WorkIsAvailable
  		case job: Job => 
  		  	val newWork = Work(job, JobID(sender, nextJobId))
		    workQ += newWork 
		    nextJobId += 1
		    log.info("Recieved new job, enqueued {}", newWork)
		    workers.foreach(_ ! WorkIsAvailable) //TODO this only once a second?
		case WorkerIdle => 
		    if(!workQ.isEmpty){
		      val work = workQ.dequeue
			  sender ! work
			  workStates = workStates + (work.jid -> PendingConfirmation(work))
			  context.system.scheduler.scheduleOnce(confirmTimeout, self, ReviewConfirmationState(work, sender))
		      log.info("Allocated JID {} to worker {}, Q size = {}", work.jid, sender, workQ.size)
		    }
		case WorkRejected(work) =>
		  	log.info("Work {} rejected by {}", work, sender)
		  	self ! work.job
		  	workStates = workStates - work.jid
		case WorkConfirmed(work) =>
		  	val jid = work.jid
		    if(workStates.contains(jid)) workStates = workStates + (jid -> InProgress(sender, work))
		    else log.warning("{} confirmed JID {} which wasn't on the work allocation list!", sender, work)
		case ReviewConfirmationState(work, worker) => //Reminder to check the allocated work was confirmed
		  workStates = workStates.get(work.jid) match{
		     case Some(PendingConfirmation(work)) =>
		       	//Work wasn't confirmed in time, so reallocate it
		    	self ! work.job
		    	workStates - work.jid
		     case Some(InProgress(_, _))=> 
		    	workStates //Everything ok
		     case None => 
		        workStates //Presume work already completed or reallocated
		   } 
		case WorkDone(work, result) =>
		  	workStates = workStates.get(work.jid) match{
		  	  case Some(InProgress(_, _)) => 
		  	    work.jid.requestor ! result
		  		log.info("{} done", work)
		  	    workStates - work.jid
		  	  case _ => 
		  	    log.error("Unexpected result {} for work {} from {}", result, work, sender)
		  	    workStates
		  	}
	    
	}
}