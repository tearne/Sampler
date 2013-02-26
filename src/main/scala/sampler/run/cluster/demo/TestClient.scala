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

package sampler.run.cluster.demo

import akka.actor.{Actor, ActorSystem, Props}
import scala.concurrent.Future
import scala.concurrent.duration._
import akka.kernel.Bootable
import akka.actor.ActorLogging
import com.typesafe.config.ConfigFactory
import sampler.run.AbortableJob
import akka.pattern.ask
import akka.pattern.pipe
import sampler.run.cluster.Master

//class SystemLoadQuery extends Actor{
//	val config = ConfigFactory.load.getConfig("testClient")
//	val numJobs = config.getInt("numJobs")
//	implicit val timeout = akka.util.Timeout(config.getInt("numJobs").minutes)
//	
//	val master = context.actorOf(Props[Master], name = "master")
//
//	def receive = {
//		
//	}
//}

class TestClientBootable extends Bootable{
	val system = ActorSystem("ClusterSystem")
	
	def startup = system.actorOf(Props[TestClientActor], name = "testClient")
	def shutdown = system.shutdown()
}

object TestClientApp extends App{
	if(args.nonEmpty) System.setProperty("akka.remote.netty.port", args(0))
	else System.setProperty("akka.remote.netty.port", "2555")
	val system = ActorSystem("ClusterSystem")
	system.actorOf(Props[TestClientActor], name = "testClient")
}

class TestClientActor extends Actor with ActorLogging{
	import context._
	import scala.concurrent.Await
	import akka.pattern.{ask, pipe}
	
	val config = ConfigFactory.load.getConfig("testClient")
	val numJobs = config.getInt("numJobs")
	implicit val timeout = akka.util.Timeout(config.getInt("numJobs").minutes)
	
	val master = actorOf(Props[Master], name = "master")
	val futures = makeJobs(numJobs).map(master ? _)
	
	def makeJobs(num: Int) = {
		(1 to num).map{i => 
			AbortableJob[String](stillRunning => {
				(1 to 100000000).foreach{i => math.sqrt(i) / math.Pi}
				Some("---"+i.toString+"---")
			})
		}
	}
	
	val f = Future.sequence(futures) mapTo manifest[IndexedSeq[String]] map(_.toString)
	f.pipeTo(self)
	
	def receive = {
		case msg => {
			log.info("*** Result ***: "+msg )
			context.system.shutdown
		}
	}
}