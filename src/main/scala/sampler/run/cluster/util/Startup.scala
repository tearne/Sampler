package sampler.run.cluster.util

import scala.sys.process.Process

object Startup extends App{

	def sshRunBackground(host:String, command: String){
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
	
	val command = "clusternfs/upload/cluster-kernel/bin/start sampler.run.cluster.WorkerBootable"
	AWS.workersPublicNames.foreach(w => sshRunBackground(w, command))
}