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

package sampler.run.agent

import akka.dispatch.{Await, Future, Promise}
import akka.actor.{ActorSystem, Actor, ActorRef, Props}
import akka.routing.RoundRobinRouter
import akka.util.Timeout
import akka.util.duration._
import akka.pattern.ask
import akka.dispatch.Promise
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable.ArrayBuffer
import sampler.run.AbortableJob
import sampler.run.AbortableRunner
import sampler.run.Abort
import sampler.run.agent.LocalActorRunner.BatchDone
import sampler.run.agent.LocalActorRunner.Batch

object Test extends App{
	val runner = new LocalActorRunner()
	
	val abort = Abort[Int](_.contains(Some(3)))
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
	
	//Create router for 4 workers
	val router = system.actorOf(Props[LocalActorRunner.Worker].withRouter(RoundRobinRouter(4)))
	val master = system.actorOf(Props(new LocalActorRunner.Master(router)))
	
	def apply[T](abort: Abort[T])(jobs: Seq[AbortableJob[T]]): Seq[Option[T]] = {
		val future = (master ? Batch(jobs, abort)).asInstanceOf[Future[BatchDone[T]]] 
		val res = Await.result(future, timeout.duration).results
		res
	}
	
	def shutdown() {
		system.shutdown
	}
}

object LocalActorRunner{
	case class Work(f: () => Option[Any])
	case class JobDone(result: Option[Any]) 
	case class Batch[T](jobs: Seq[AbortableJob[T]], abort: Abort[T]){
		val size = jobs.size
		def shouldAbort(soFar: Seq[Any]) = abort(soFar.asInstanceOf[Seq[Option[T]]])	
	}
	case class BatchDone[T](results: Seq[Option[T]])
	
	class Worker extends Actor{
		def receive = {
			case w: Work => sender ! JobDone(w.f())
			case _ => throw new UnsupportedOperationException("Unexpected message")
		}
	}
	
	class Master(router: ActorRef) extends Actor{
		import context.dispatcher
		
		var running = new AtomicBoolean(false)
		var pending: Int = 0
		var requester: ActorRef = _
		var acc = ArrayBuffer[Option[Any]]()
		var batch : Batch[_] = _
		
		def receive = {
			case b: Batch[_] => 
				if(!running.get()) {
					running = new AtomicBoolean(true)
					b.jobs.foreach(job => router ! Work{
						() => job.run(running)
					})
					pending = b.size
					requester = sender
					batch = b
				} else {
					println("Working on another job, ignoring.")
				}
			case jd: JobDone =>
				if(running.get){
					acc += jd.result
					pending = pending - 1
					if(pending == 0) done()
					else if(batch.shouldAbort(acc)) abort()
				}
			case _ => throw new UnsupportedOperationException("Unexpected message")
		}
		
		def done() = {
			pending = 0
			requester ! BatchDone[Any](acc)
			running.set(false)
			acc = ArrayBuffer[Option[Any]]()
		}
		
		def abort() = {			
			println("Aborting")
			done()
		}
	}
}