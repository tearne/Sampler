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

package sampler.run.actor

import akka.actor.{ActorSystem => AkkaSystem}
import scala.util.Try
import org.jboss.netty.channel.ChannelException
import org.slf4j.LoggerFactory
import com.typesafe.config.ConfigFactory

object PortFallbackSystem {
	val log = LoggerFactory.getLogger(PortFallbackSystem.this.getClass())
	
	def apply(name: String): AkkaSystem = {
//		try{
//			AkkaSystem(name)
//		} catch {
//			case e: ChannelException =>
//				log.warn("Failed to bind to configured port, falling back to random: "+e.getLocalizedMessage())
//				System.setProperty("akka.remote.netty.port", "0")
//				ConfigFactory.invalidateCaches()
//				AkkaSystem(name)
//			case e: Throwable => 
//				log.error("Unexpected error: "+e.printStackTrace())
//				throw new RuntimeException("Unexpected error on ActorSystem startup", e)
//		}
		
		val startPort = ConfigFactory.load.getInt("akka.remote.netty.port")
		
		def tryPort(i: Int) = {
			System.setProperty("akka.remote.netty.port", i.toString)
			ConfigFactory.invalidateCaches()
			Try(AkkaSystem())
		}

		Try(AkkaSystem(name))
			.orElse(tryPort(startPort + 1))
			.orElse(tryPort(startPort + 2))
			.orElse(tryPort(0))
			.get
			
	}
}