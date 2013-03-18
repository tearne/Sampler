package sampler.run.cluster.util

import scala.sys.process.Process
import sampler.run.cluster.util.AWS

object SSHCommand {
	def apply(host:String, command: String){
		Process("ssh", List(
			"-t",
			"-t",
			"-i",
			AWS.keyFile.toString,
			"-o",
			"StrictHostKeyChecking=no",
			"-o",
			"UserKnownHostsFile=/dev/null",
			"ec2-user@%s".format(host),
			command
		)).!
	}
	
	def background(host:String, command: String){
		Process("ssh", List(
			"-f",
			"-n",
			"-i",
			AWS.keyFile.toString,
			"-o",
			"StrictHostKeyChecking=no",
			"-o",
			"UserKnownHostsFile=/dev/null",
			"ec2-user@%s".format(host),
			"""sh -c 'nohup """+command+""" > /dev/null 2>&1 &'"""
			//command+" &"
		)).!
	}
}