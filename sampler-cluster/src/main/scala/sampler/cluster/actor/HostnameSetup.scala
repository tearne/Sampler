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

package sampler.cluster.actor

import org.slf4j.LoggerFactory
import com.typesafe.config.ConfigFactory
import scala.util.{Try, Success, Failure}

trait HostnameSetup {
	val log = LoggerFactory.getLogger(this.getClass())
	
	if(ConfigFactory.load().getBoolean("cluster.inet-bind")){
		Try{
			java.net.InetAddress.getLocalHost.getHostAddress
		}match{
			case Success(addr) => 
				System.setProperty("akka.remote.netty.hostname", addr)
				log.info("Binding to local host address "+addr)
			case Failure(_) => 
				log.warn("Falling back to config hostname instead of inet host address")
		}
	} else log.info("Binding with hostname in config")
	ConfigFactory.invalidateCaches()
}