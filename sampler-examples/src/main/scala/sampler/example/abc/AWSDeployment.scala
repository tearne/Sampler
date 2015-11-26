package sampler.example.abc

import java.nio.file.Paths
import com.amazonaws.services.ec2.AmazonEC2Client
import com.amazonaws.auth.PropertiesCredentials
import java.nio.file.Files
import com.amazonaws.services.ec2.model.DescribeInstancesRequest
import scala.collection.JavaConversions._
import sampler.io.Logging
import com.amazonaws.services.ec2.model.Tag
import com.amazonaws.services.identitymanagement.AmazonIdentityManagementClient
import java.util.concurrent.TimeUnit
import java.nio.file.Path
import scala.sys.process._
import sampler.cluster.deploy.SSH
import java.nio.file.StandardOpenOption
import java.nio.charset.Charset
import java.nio.file.attribute.PosixFilePermission
import sampler.cluster.deploy.AWSProperties
import sampler.cluster.deploy.AWS
import com.amazonaws.services.ec2.model.Instance

/*
 * Assuming a number of EC2 images with the base software (JRE7, R, etc)
 * have already been spun up.  To do this see the notes in 
 * sampler-examples/deploy/notes.txt
 * 
 * To run the deployment program set a property like the following
 * in the run configuration
 *     -DawsProperties=/mnt/hgfs/share/AWS/aws.properties.txt
 *     
 *     
 * There are several places where the configuration is set:
 * 
 * 1) This file
 *   - 'application' name, e.g. sampler.example.abc.FlockMortality
 *   - 'tearDown' = true/false, to determine whether to add nodes to
 *     an existing cluster, or destroy and re-deploy everything
 *   - Deploy directory, e.g. "~/deploy/"
 *   - Java 'runCommand', java -Xmx3g ...
 * 
 * 2) The awsProperties file (see src/main/resources/AWS for example)
 *   - accessKey = XXXXXXXXXXXXXXXXXXXX
 *   - secretKey = XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 *   - endpoint = ec2.eu-west-1.amazonaws.com
 *   - tagName = Name
 *   - masterTag = master
 *   - workerTag = worker
 *   - instanceUserName = ec2-user
 *   - s3Bucket = s3://some-bucket
 *   - s3cfgPath = s3cfg-file		
 *   - sshKeyPath = sshKey.pem
 * 
 * 3) The s3cfg file (location described in awsProperties), which is copied 
 *    over to the instances so they can download the deployment files
 *   - Contains AWS-S3 access key
 *   - (see src/main/resources/AWS for example)
 * 
 * 3) deploy/application.config for the deployed application
 *   - Degree of parallelism (akka.actor.deployment./root/work-router.nr-of-instances)
 *   - Number of particles, generation, chunk size, mix rate, ...
 *   - Log level for the Akka actors
 * 
 * 4) deploy/copyJars.sh to aggregate the deployment jars in the deployment 
 *    directory, ready to upload to s3
 * 
 * 5) deploy/logging.xml for deployed application
 *   - Log files and levels for non-Akka components
 *  
 * 
 * How to Deploy
 *  1) Run 'sbt package' on project to build jars
 *  2) Run Sampler/sampler-examples/deploy/copyJars.sh to pull in all jars to
 *     deploy/lib dir
 *  3) Check application.conf and logback.xml
 *  4) Check and run localTest.sh
 *  5) Run this script
 *  6) Log into a worker (not the master).  May need to stop the program and 
 *     modify the application.conf, e.g. to set it to terminate and report at 
 *     the right time.
 * 
 * 
 * TODO
 *  - If an instance isn't booted up yet the commands will silently fail
 *    and it will look like everything worked fine.  Would be nicer to use
 *    exit codes for reporting at the end.
 *  - Config is too scattered (as above)
 */
object ClusterDeployment extends App with CommonDeployTasks {
	import awsProps._
	
	/*
	 * Configuration
	 */
	val tearDown = true
	//
	//
	//
	//  !!!!!!!!!
	//
	//
	//
	val application: String = "sampler.example.old.Network"
	def getRunScript(localPrivateIP: String, masterPrivateIP: String) = s"""
java -Xmx3g \\
-Dakka.remote.netty.tcp.hostname=$localPrivateIP \\
-Dakka.cluster.seed-nodes.0=akka.tcp://ABC@$masterPrivateIP:2552 \\
-Dconfig.file=deploy/application.conf \\
-Dlogback.configurationFile=deploy/logback.xml \\
-cp "deploy/lib/*" \\
$application
"""

	if(tearDown){
		// Stop everything
		(aws.masterNode +: aws.workerNodes).foreach(node => killJava(node))
	}

	val masterNode = aws.masterNode
	val masterPrivateIP = masterNode.getPrivateIpAddress()
	
	def deploy(node: Instance){
		if(tearDown || !javaRunning(node)){
			aws.scpUpload(s3CfgPath, node, "~/.s3cmd")
			aws.s3RemoteDownload(node, "~/deploy/")
			val runScriptName = uploadScript(node, getRunScript(node.getPrivateIpAddress(),	masterPrivateIP))
			runRemoteScript(node, runScriptName)
			info("started application on "+node.getPrivateIpAddress())
		}
	}
	
	(masterNode +: aws.workerNodes).foreach{node => deploy(node)}
	
	info(s"----==== Cluster ready captain ====----")
	aws.workerNodes.foreach{node =>
		info(s"      Worker: ssh -i ${awsProps.sshKeyPath} ec2-user@${node.getPublicDnsName}")
	}
	info(s" ***  Master: ssh -i ${awsProps.sshKeyPath} ec2-user@${masterNode.getPublicDnsName}")
}

trait CommonDeployTasks extends Logging{
	def runRemoteScript(node: Instance, fileName: String){
		val cmd = ssh.backgroundCommand(awsProps.instanceUserName, node.getPublicDnsName(), s"~/deploy/$fileName")
		info(cmd)
		Process(cmd).!
	}
	
	def javaRunning(node: Instance): Boolean = {
		val cmd = ssh.forgroundCommand(awsProps.instanceUserName, node.getPublicDnsName(), s"pgrep -f java")
		val result = 0 == Process(cmd).!
		info("Java running $result:  $cmd")
		result
	}
	
	def killJava(node: Instance){
		val cmd = ssh.backgroundCommand(awsProps.instanceUserName, node.getPublicDnsName(), "killall java")
		info(cmd)
		Process(cmd).!
	}
		
	def uploadScript(node: Instance, script: String): String = {
		val publicDNS = node.getPublicDnsName
		val privateIP = node.getPrivateIpAddress
		
		val tempFile = Files.createTempFile("run", ".sh")
		tempFile.toFile().deleteOnExit
		Files.setPosixFilePermissions(tempFile, Set(
				PosixFilePermission.OWNER_EXECUTE,
				PosixFilePermission.OWNER_READ,
				PosixFilePermission.OWNER_WRITE,
				PosixFilePermission.GROUP_EXECUTE,
				PosixFilePermission.OTHERS_EXECUTE
		))
		
		val writer = Files.newBufferedWriter(tempFile, Charset.defaultCharset())
		writer.write("#!/bin/bash")
		writer.write(script)
		writer.close
		
		aws.scpUpload(tempFile, node, "~/deploy/"+tempFile.getFileName())	
		tempFile.getFileName().toString
	}

	val localDeployDir = Paths.get(System.getProperty("user.home")).resolve("Sampler/sampler-examples/deploy/")
	assert(Files.exists(localDeployDir))	
	info(s"localDeployDir: ${localDeployDir}")
	
	val awsProps = AWSProperties.load
	val ssh = new SSH(awsProps.sshKeyPath)
	val aws = new AWS(awsProps)
	info(""+awsProps)
	
	info(s"Account Confirmed: ${aws.getUserDetails}")
	aws.s3Upload(localDeployDir.toAbsolutePath)
}
