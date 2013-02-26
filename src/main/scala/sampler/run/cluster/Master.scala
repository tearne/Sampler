/*
 * Copyright (c) 2013 Crown Copyright 
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

package sampler.run.cluster

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.cluster.Cluster
import scala.collection._
import scala.concurrent.duration._
import sampler.run.AbortableJob
import sampler.math.Random
import scala.concurrent.Promise
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import sampler.run.Job


case class CheckStarted(work: Work, worker: ActorRef)
sealed trait WorkState
case class Starting(work: Work) extends WorkState
case class InProgress(worker: ActorRef, work: Work) extends WorkState
case class BroadcastWork()

class Master extends Actor with ActorLogging{
	val workQueue = mutable.Queue.empty[Work]

	//We have to do a dance with JobIDs since can't expect Jobs to pass equality checks
	//TODO Should be able to now?
	val jobStates = mutable.Map.empty[JobID, WorkState] 
	
	val confirmationTimeout = 1.seconds
	var nextJobIndex = 0
	
	val injector = context.actorOf(Props[Injector])
	import context.dispatcher
	context.system.scheduler.schedule(1.seconds, 5.second, self, BroadcastWork)
	//context.system.scheduler.scheduleOnce(5.seconds, self, BroadcastWork)
	
	import context._
  
	def receive = {
  		case IsWorkAvailable => 
<<<<<<< HEAD
  			if(!jobQueue.isEmpty){ 
  				log.info("Informing {} that work is available", sender)
  				sender ! WorkIsAvailable
  			}
  		case parameters: JobParameters => 
  		  	val newJob = Job(parameters, JobID(sender, nextJobId))
		    jobQueue += newJob 
		    nextJobId += 1
		    log.info("Recieved new job, enqueued {}", newJob)
		    injector ! Broadcast(WorkIsAvailable) //TODO this only once a second?
  		case NewWorker(w) => self.tell(WorkerIsIdle, w)	//Assuming that new workers are idle initially
  		case WorkerDown(w) => 
  			log.info("Current work table is {}", jobStates)
  			val lostWork = for(InProgress(worker, job) <- jobStates.values if worker == w) yield job
  		  	lostWork.foreach{job =>
  		  	  self tell(job.parameters, job.id.requestor)
  		  	  jobStates -= job.id
  		  	  log.info("Reallocated {} from unreachable worker {}", job, w)  		  	  
  		  	}
  		case WorkerIsIdle => 
		    if(!jobQueue.isEmpty){
		      val job = jobQueue.dequeue
			  sender ! job
			  jobStates += (job.id -> Starting(job))
			  context.system.scheduler.scheduleOnce(confirmationTimeout, self, CheckStarted(job, sender))
		      log.info("Allocated {} to worker {}, Q size now {}", job, sender, jobQueue.size)
		    }
		case JobRejected(job) =>
		  	log.info("Job {} rejected by {}, resubmitting to queue.", job, sender)
		  	self ! job.parameters
		  	jobStates -= job.id
		case JobConfirmed(job) =>
		  	val jid = job.id
		  	if(!jobStates.contains(jid))
		  	  log.warning("{} confirmed {} which wasn't allocated!", sender, job)
		  	else jobStates.update(jid, InProgress(sender, job))
		case CheckStarted(job, worker) => //Reminder to check the allocated work was confirmed
		  jobStates.get(job.id).foreach{
		    case Starting(work) =>
		      //Work wasn't confirmed in time, so reallocate it
		    	self ! job.parameters
		    	jobStates - job.id
=======
  			if(!workQueue.isEmpty){ 
  				log.debug("Informing {} that work is available", sender)
  				sender ! WorkIsAvailable
  			}
  		case job: Job[_] => 
  			val requestor = sender
  			val index = nextJobIndex
  			nextJobIndex += 1
  		  	val newWork = Work(job, JobID(requestor, index))
  		  	workQueue += newWork 
  		  	log.info("New job enqueued, id {}, |Q|={}", newWork, index, workQueue.size)
  		case BroadcastWork =>
		    if(!workQueue.isEmpty) injector ! Broadcast(WorkIsAvailable) //TODO this only once a second?
  		case NewWorker(w) => self.tell(WorkerIsIdle, w)	//Assuming that new workers are idle initially
  		case WorkerDown(w) => 
  			log.debug("Current work table is {}", workStates)
  			val lostWork = for(InProgress(worker, work) <- workStates.values if worker == w) yield work
  		  	lostWork.foreach{work =>
  		  	  self tell(work.job, work.jid.requestor)
  		  	  workStates -= work.jid
  		  	  log.warning("Reallocated {} from unreachable worker {}", work, w)  		  	  
  		  	}
  		case WorkerIsIdle => 
  			val worker = sender
		    if(!workQueue.isEmpty){
		      val work = workQueue.dequeue
			  sender ! work
			  workStates += (work.jid -> Starting(work))
			  context.system.scheduler.scheduleOnce(confirmationTimeout, self, CheckStarted(work, worker))
			  log.info("Allocated {} to {}, Q size now {}", work, worker, workQueue.size)
		    }
		case WorkRejected(work) =>
			val worker = sender
		  	log.info("Work {} rejected by {}, resubmitting to queue.", work, worker)
		  	self.tell(work.job, work.jid.requestor)
		  	workStates -= work.jid
		case WorkConfirmed(work) =>
			val worker = sender
		  	val jid = work.jid
		  	if(!workStates.contains(jid))
		  	  log.warning("{} confirmed {} which wasn't allocated!", worker, work)
		  	else workStates.update(jid, InProgress(sender, work))
		case CheckStarted(work, worker) => //Reminder to check the allocated work was confirmed
		  workStates.get(work.jid).foreach{
		    case Starting(work) =>
		      	//Work wasn't confirmed in time, so reallocate it
		    	log.info("Work {} not started in time, resubmitting.", work)
		    	self ! work.job
		    	workStates - work.jid
>>>>>>> oliver
		    case _ =>
		  }
		case JobDone(job, result) =>
		  jobStates.get(job.id) match{
		  	  case Some(InProgress(_, _)) => 
<<<<<<< HEAD
		  	    job.id.requestor ! result
		  		log.info("{} done", job)
		  	    jobStates -= job.id
		  	    log.info("Work states {}", jobStates)
=======
		  	    work.jid.requestor ! result
		  		log.info(
		  				"Job {} done, informed requestor: {}", 
		  				work.jid.allocId, 
		  				work.jid.requestor
		  		)
		  	    workStates -= work.jid
		  	    log.debug("Work states {}", workStates)
>>>>>>> oliver
		  	  case _ => 
		  	    log.error("Unexpected result {} for work {} from {}", result, job, sender)
		  	}
	}
}