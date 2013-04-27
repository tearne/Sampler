package sampler.run.aws

import scala.sys.process.Process

object SSHCommand {
	def apply(username:String, host:String, command: String){
		Process("ssh", List(
			"-t",
			"-t",
			"-i",
			AWS.keyFile.toString,
			"-o",
			"StrictHostKeyChecking=no",
			"-o",
			"UserKnownHostsFile=/dev/null",
			s"$username@$host",
			command
		)).!
	}
	
	def background(username:String, host:String, command: String){
		Process("ssh", List(
			"-f",
			"-n",
			"-i",
			AWS.keyFile.toString,
			"-o",
			"StrictHostKeyChecking=no",
			"-o",
			"UserKnownHostsFile=/dev/null",
			s"$username@$host",
			"""sh -c 'nohup """+command+""" > /dev/null 2>&1 &'"""
			//command+" &"
		)).!
	}
}