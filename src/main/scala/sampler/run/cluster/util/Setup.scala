package sampler.run.cluster.util

import java.nio.file.Paths
import scala.sys.process.Process
import scala.collection.JavaConversions._
import java.nio.file.Path
import java.nio.file.Files
import java.nio.charset.Charset

object Setup extends App{
	val scriptsBase = Paths.get("/mnt/hgfs/EC2/setupScripts")
	
	def sshRun(host:String, command: String){
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
	
	def loadScript(path: Path): String = 
		Files.readAllLines(path, Charset.defaultCharset())
		.filter(line => !(line.startsWith("#") || line.trim.isEmpty))
		.reduce(_ + ';' + _)
	
	val basicScript = loadScript(scriptsBase.resolve("basicSetup.sh"))
	//println(AWS.allNodeNames)
	AWS.allNodeNames.foreach(i => sshRun(i, basicScript))
	
	val nfsServerScript = loadScript(scriptsBase.resolve("nfsServerSetup.sh"))
	sshRun(AWS.masterPublicName, nfsServerScript)
	
	val nfsMountScript = loadScript(scriptsBase.resolve("nfsMountSetup.sh")).replaceAll("\\$HOST", AWS.masterPublicName)
	println(nfsMountScript)
	AWS.allNodeNames.foreach(i =>  sshRun(i, nfsMountScript))
}