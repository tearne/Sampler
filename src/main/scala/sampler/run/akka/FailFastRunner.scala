/*
 * Copyright (c) 2012-13 Crown Copyright 
 *                       Animal Health and Veterinary Laboratories Agency
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

package sampler.run.akka

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Try
import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import sampler.run.ActorJob
import sampler.run.ActorJobRunner
import akka.actor.Props
import sampler.run.akka.client.failfast.FailFastActor
import sampler.run.akka.client.failfast.Batch

class FailFastRunner(system: ActorSystem) extends ActorJobRunner{
	implicit val timeout = Timeout(5.minutes)
	import system.dispatcher
	
	def apply[R](jobs: Seq[ActorJob[R]]): Seq[Try[R]] = {
		val ff = system.actorOf(Props[FailFastActor])
		Await.result((ff ? Batch(jobs)).mapTo[Seq[Try[R]]], timeout.duration)
	}
}



