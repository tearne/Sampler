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

class Master extends Actor with ActorLogging{
	case class CheckStarted(work: Work, worker: ActorRef)
	
	sealed trait WorkState
	case class Starting(work: Work) extends WorkState
	case class InProgress(worker: ActorRef, work: Work) extends WorkState
  
	val workQueue = mutable.Queue.empty[Work]
	//We have to do a dance with JobIDs since can't expect Jobs to pass equality checks
	val workStates = mutable.Map.empty[JobID, WorkState] 
	
	val confirmationTimeout = 1.seconds
	var nextJobId = 0
	
	val injector = context.actorOf(Props[Injector])
	
	import context._
  
	def receive = {
  		case IsWorkAvailable => 
  			if(!workQueue.isEmpty){ 
  				log.info("Informing {} that work is available", sender)
  				sender ! WorkIsAvailable
  			}
  		case job: Job[Any, Random] => 
  		  	val newWork = Work(job, JobID(sender, nextJobId))
		    workQueue += newWork 
		    nextJobId += 1
		    log.info("Recieved new job, enqueued {}", newWork)
		    injector ! Broadcast(WorkIsAvailable) //TODO this only once a second?
  		case NewWorker(w) => self.tell(WorkerIsIdle, w)	//Assuming that new workers are idle initially
  		case WorkerDown(w) => 
  			log.info("Current work table is {}", workStates)
  			val lostWork = for(InProgress(worker, work) <- workStates.values if worker == w) yield work
  		  	lostWork.foreach{work =>
  		  	  self tell(work.job, work.jid.requestor)
  		  	  workStates -= work.jid
  		  	  log.info("Reallocated {} from unreachable worker {}", work, w)  		  	  
  		  	}
  		case WorkerIsIdle => 
		    if(!workQueue.isEmpty){
		      val work = workQueue.dequeue
			  sender ! work
			  workStates += (work.jid -> Starting(work))
			  context.system.scheduler.scheduleOnce(confirmationTimeout, self, CheckStarted(work, sender))
		      log.info("Allocated {} to worker {}, Q size now {}", work, sender, workQueue.size)
		    }
		case WorkRejected(work) =>
		  	log.info("Work {} rejected by {}, resubmitting to queue.", work, sender)
		  	self ! work.job
		  	workStates -= work.jid
		case WorkConfirmed(work) =>
		  	val jid = work.jid
		  	if(!workStates.contains(jid))
		  	  log.warning("{} confirmed {} which wasn't allocated!", sender, work)
		  	else workStates.update(jid, InProgress(sender, work))
		case CheckStarted(work, worker) => //Reminder to check the allocated work was confirmed
		  workStates.get(work.jid).foreach{
		    case Starting(work) =>
		      //Work wasn't confirmed in time, so reallocate it
		    	self ! work.job
		    	workStates - work.jid
		    case _ =>
		  }
		case WorkDone(work, result) =>
		  workStates.get(work.jid) match{
		  	  case Some(InProgress(_, _)) => 
		  	    work.jid.requestor ! result
		  		log.info("{} done", work)
		  	    workStates -= work.jid
		  	    log.info("Work states {}", workStates)
		  	    //sender ! WorkIsAvailable
		  	  case _ => 
		  	    log.error("Unexpected result {} for work {} from {}", result, work, sender)
		  	}
	}
}