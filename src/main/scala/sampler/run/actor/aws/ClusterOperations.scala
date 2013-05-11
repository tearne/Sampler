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

package sampler.run.actor.aws

import scala.sys.process.Process
import java.nio.file.{Paths, Path, Files}

object ClusterOperations extends App{

	//TODO Chain processes
	
	val tagName = "Name"
	val tagValue = "ot"
	val username = "ec2-user"
	val payload = Paths.get("target","cluster-kernel")
	val payloadExe = "cluster-kernel/bin/start sampler.run.cluster.WorkerBootable"
	
		
		
	val s3bucket = s"s3://ot-bucket/"//${payload.getFileName()}"
//	val s3Payload = s"s3://$s3bucket/${payload.getFileName()}"
	
	val nodes = AWS.clusterNodes(tagName, tagValue)
	
	nodes.foreach(println)
//	nodes.foreach(host => installBasics(host))
//	emptyS3Bucket(s3bucket)
	doS3upload(payload, s3bucket)
	nodes.foreach(host => stopPayload(host))
//	nodes.foreach(host => resetInstance(host))
//	nodes.foreach(host => directUpload(host, Paths.get("/home/user/.s3cfg")))
	nodes.foreach(host => doS3download(host, s3bucket+payload.getFileName()))
	nodes.foreach(host => runPayload(host))
	
	nodes.foreach(println)
	
	def directUpload(host: String, file: Path){
		val cmd = s"scp -i ${AWS.keyFile.toString} -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null $file $username@$host:~"
		println(cmd)
		Process(cmd).!
	}
	
	def emptyS3Bucket(bucket: String){
		val command = s"s3cmd del s3://$bucket/*"
		Process(command).!
	}
	
	//TODO gives no feedback in console
	def doS3upload(localPath: Path, s3Target: String){
		val command = s"s3cmd sync --delete-removed${if(Files.isDirectory(localPath))" --recursive" else ""} $localPath $s3Target"
		println(command)
		Process(command).!
	}
	
	def resetInstance(host: String){
		val resetInstanceCommand = """killall java; rm -r ~/*"""
		SSHCommand(username, host, resetInstanceCommand)
	}
	
	def stopPayload(host: String){
		val resetInstanceCommand = """killall java"""
		SSHCommand(username, host, resetInstanceCommand)
	}
	
	def installBasics(host: String){
		val script = 
"""
# Java
sudo yum -y install java-1.7.0
echo 2 | sudo alternatives --config java
# s3cmd + don't forget to --configure
cd /etc/yum.repos.d
sudo wget http://s3tools.org/repo/RHEL_6/s3tools.repo
sudo yum install s3cmd -y
"""
		SSHCommand(username, host, script)
	}
	
	def doS3download(host: String, s3Path: String){
		val s3downloadCommand = s"s3cmd sync $s3Path ."
		println(s3downloadCommand)
		SSHCommand(username, host, s3downloadCommand)
		SSHCommand(username, host, "chmod u+x cluster-kernel/bin/start")
	}
	
	def runPayload(host: String){
		SSHCommand.background(username, host, payloadExe)
	}
}