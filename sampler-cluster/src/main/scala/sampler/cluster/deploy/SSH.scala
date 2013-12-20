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

package sampler.cluster.deploy

import scala.sys.process.{Process, ProcessBuilder}
import sampler.cluster.deploy.aws.AWS
import java.nio.file.Path
import sampler.io.Logging
import scala.language.postfixOps

class SSH(keyFile: Option[Path] = None) extends Logging{
	private val keyFileArgs = keyFile.map(f => List("-i", f.toString)).getOrElse(Nil)
	private val noHostFileArgs = List(
			"-o", "StrictHostKeyChecking=no",
			"-o","UserKnownHostsFile=/dev/null",
			"-o", "LogLevel=quiet"
	)
	
	def forground(username: String, host: String, command: String) {
		val args = List("-t","-t") ::: 
			keyFileArgs :::
			noHostFileArgs :::
			List(
				s"$username@$host",
				command
			)
		
		Process("ssh", args).!!
	}
	
	def background(username: String, host: String, command: String) {
		val args = 
			List("-f","-n") ::: 
			keyFileArgs ::: 
			noHostFileArgs :::
			List(
				s"$username@$host",
				"""sh -c 'nohup """+command+""" > /dev/null 2>&1 &'"""
			)
		
		log.info("Running: ssh {}",args.map(_+" ").mkString)
		log.info("Output: {}",Process("ssh", args).!!)
	}
	
	def scp(username: String, host: String, file: Path){
		val args = keyFileArgs ::: 
			noHostFileArgs ::: 
			List(s"$file", s"$username@$host:~")
		
		println(args.map(_ +' ').mkString)
		val p = Process("scp", args)
		println(p.toString)
		p.!!
	}
}