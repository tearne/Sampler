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

package sampler.cluster

import scala.util.Try
import com.typesafe.config.ConfigFactory
import akka.actor.{ActorSystem => AkkaSystem}
import sampler.io.Logging
import akka.actor.{ActorSystem => AkkaSystem}

object PortFallbackSystemFactory extends Logging{
	def apply(name: String): AkkaSystem = {
		val startPort = ConfigFactory.load.getInt("akka.remote.netty.tcp.port")
		
		def tryPort(i: Int) = {
			System.setProperty("akka.remote.netty.tcp.port", i.toString)
			ConfigFactory.invalidateCaches()
			Try(AkkaSystem(name))
		}

		Try(AkkaSystem(name))
			.orElse(tryPort(startPort + 1))
			.orElse(tryPort(startPort + 2))
			.orElse(tryPort(0))
			.get
	}
}