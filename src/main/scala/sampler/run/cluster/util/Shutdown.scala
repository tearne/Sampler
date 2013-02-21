package sampler.run.cluster.util

import scala.sys.process.Process

object Shutdown extends App{

	def sshRunBackground(host:String, command: String){
		Process("ssh", List(
			"-f",
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
	
	val command = "killall java"
	AWS.workersPublicNames.foreach(w => sshRunBackground(w, command))
}