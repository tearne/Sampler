/*
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

package sampler.run.actor

import akka.dispatch.{Await, Future, Promise}
import akka.actor.{ActorSystem, Actor, ActorRef, Props}
import akka.routing.RoundRobinRouter
import akka.util.Timeout
import akka.util.duration._
import akka.pattern.ask
import akka.dispatch.Promise
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable.ArrayBuffer
import sampler.run.{AbortableJob, AbortableRunner, AbortFunction}
import sampler.run.actor.LocalActorRunner.{Batch, BatchDone}

object Test extends App{
	val runner = new LocalActorRunner()
	
	val abort = AbortFunction[Int](_.contains(Some(3)))
	val jobs = (1 to 10).map(i => AbortableJob(
		(stillRunning: AtomicBoolean) => {
			(1 to 5).find{j =>
				if(stillRunning.get){
					Thread.sleep(100); 
					println("Job "+i)
					false
				}else{
					println("Job "+i+" bailing out")
					true
				}
			}
			println("Job "+i+" complete")
			Some(i)
		}
	))
	
	println(runner(abort)(jobs))
	
//	runner.shutdown
}


class LocalActorRunner extends AbortableRunner{
	val system = ActorSystem("MasterSystem")
	implicit val timeout = Timeout(5 minutes)
	
	// Gets number of available processors, makes Runner generic
	val cores = Runtime.getRuntime().availableProcessors()
	
	val router = system.actorOf(Props[LocalActorRunner.Worker].withRouter(RoundRobinRouter(cores)))
	val master = system.actorOf(Props(new LocalActorRunner.Master(router)))
	
	def apply[T](abort: AbortFunction[T])(jobs: Seq[AbortableJob[T]]): Seq[Option[T]] = {
		val future = (master ? Batch(jobs, abort)).asInstanceOf[Future[BatchDone[T]]] 
		val res = Await.result(future, timeout.duration).results
		res
	}
	
	def shutdown() {
		system.shutdown
	}
}

object LocalActorRunner{
	case class Work(f: () => Option[Any], parentBatch: Batch[_])
	case class JobDone(result: Option[Any], parentBatch: Batch[_]) 
	case class Batch[T](jobs: Seq[AbortableJob[T]], abort: AbortFunction[T], var isOperational: AtomicBoolean = new AtomicBoolean(true)){
		val size = jobs.size
		def shouldAbort(soFar: Seq[Any]) = abort(soFar.asInstanceOf[Seq[Option[T]]])	
	}
	case class BatchDone[T](results: Seq[Option[T]])
	
	class Worker extends Actor{
		def receive = {
			case w: Work => sender ! JobDone(w.f(), w.parentBatch)
			case _ => throw new UnsupportedOperationException("Unexpected message")
		}
	}
	
	class Master(router: ActorRef) extends Actor{
		import context.dispatcher
		
		var currentBatch: Option[Batch[_]] = None
		var pending: Int = 0
		var requester: ActorRef = _
		var acc = ArrayBuffer[Option[Any]]()
		
		def receive = {
			case b: Batch[_] => 
				if(!currentBatch.isDefined) {
					b.jobs.foreach(job => router ! 
						Work(
								() => job.run(b.isOperational),
								b
						)
					)
					pending = b.size
					requester = sender
					currentBatch = Some(b)
				} else {
					println("Working on another job, ignoring request.")
				}
			case jd: JobDone =>
				if(currentBatch.isDefined && currentBatch.get == jd.parentBatch){
					acc += jd.result
					pending = pending - 1
					if(pending == 0) done()
					else if(currentBatch.get.shouldAbort(acc)) abort()
				}
//				else {
//					println("Ignoring late result")
//				}
			case _ => throw new UnsupportedOperationException("Unexpected message")
		}
		
		def done() = {
			pending = 0
			requester ! BatchDone[Any](acc)
			currentBatch.get.isOperational = new AtomicBoolean(false)
			currentBatch = None
			acc = ArrayBuffer[Option[Any]]()
		}
		
		def abort() = {			
			println("Aborting")
			done()
		}
	}
}