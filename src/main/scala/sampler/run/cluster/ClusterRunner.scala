/*
 * Copyright (c) 2012 Crown Copyright 
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

import akka.actor.ActorSystem
import akka.util.Timeout
import akka.actor.Props
import akka.pattern.ask
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import sampler.run.Job
import sampler.run.JobRunner
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import com.typesafe.config.ConfigFactory

class Runner() extends JobRunner{
	import scala.concurrent.duration._
	
	//TODO this messes up later config loads in Akka
	//if(ConfigFactory.load().getBoolean("cluster.inet-bind")){
		Try{
			java.net.InetAddress.getLocalHost.getHostAddress
		}match{
			case Success(addr) => 
				System.setProperty("akka.remote.netty.hostname", addr)
				println("Binding to "+addr)
			case Failure(_) => println("Using config hostname")
		}
	//} else println("Binding to localhost")
	
	val system = ActorSystem("ClusterSystem")
	implicit val timeout = Timeout(5.minutes)
	
	val master = system.actorOf(Props[Master], name = "master")
	import system.dispatcher
	
	def apply[T](jobs: Seq[Job[T]]): Seq[Option[T]] = {
		val futures = jobs.map(master ? _)
		
		val fSeq = Future.sequence(futures).mapTo[Seq[Option[T]]]
		
		val res = Await.result(fSeq, timeout.duration)
		res
	}
	
	def shutdown() {
		system.shutdown
	}
}