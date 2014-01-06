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
import sampler.cluster.run.ClusterNode
import sampler.cluster.deploy.SSH
import java.nio.file.StandardOpenOption
import java.nio.charset.Charset
import java.nio.file.attribute.PosixFilePermission
import sampler.cluster.deploy.AWSProperties
import sampler.cluster.deploy.AWS
import com.amazonaws.services.ec2.model.Instance

trait CommonDeployment extends Logging{
	val newLine = System.getProperty("line.separator")
	
		def createRemoteRunScript(node: Instance, masterIP: String): String = {
		val publicDNS = node.getPublicDnsName
		val privateIP = node.getPrivateIpAddress
		
		val runCommand = 
s"""
java -Xmx2g \\
-Dakka.remote.netty.tcp.hostname=$privateIP \\
-Dakka.cluster.seed-nodes.0=akka.tcp://ABC@$masterIP:2552 \\
-Dconfig.file=application.conf \\
-Dlogback.statusListenerClass=ch.qos.logback.core.status.OnConsoleStatusListener \\
-Dlogback.configurationFile=logback.xml \\
-cp "lib/*" \\
sampler.example.abc.FlockMortality
"""

		val tempFile = Files.createTempFile("run", ".sh")
		tempFile.toFile().deleteOnExit
		log.info("Temp file path is {}", tempFile)
		Files.setPosixFilePermissions(tempFile, Set(
				PosixFilePermission.OWNER_EXECUTE,
				PosixFilePermission.OWNER_READ,
				PosixFilePermission.OWNER_WRITE,
				PosixFilePermission.GROUP_EXECUTE,
				PosixFilePermission.OTHERS_EXECUTE
		))
		
		val writer = Files.newBufferedWriter(tempFile, Charset.defaultCharset())
		writer.write("#!/bin/bash")
		writer.write(runCommand)
		writer.close
		
		ssh.scp(awsProps.instanceUserName, publicDNS, tempFile)	
		tempFile.getFileName().toString
	}

	/*
	 * 
	 * To prepare the deployment directory
	 *  1) run 'sbt package' on project to build jars
	 *  2) run Sampler/sampler-examples/deploy/copyJars.sh to pull in all jars to deploy/lib dir
	 *  3) Check application.conf and logback.xml
	 *  4) Check and run localTest.sh
	 * 
	 */
	val localDeployDir = Paths.get(System.getProperty("user.home")).resolve("Sampler/sampler-examples/deploy/")
	assert(Files.exists(localDeployDir))	
	log.info("localDeployDir: {}", localDeployDir)
	
	val awsProps = AWSProperties.load
	val ssh = new SSH(awsProps.sshKeyPath)
	val aws = new AWS(awsProps)
	log.info(""+awsProps)
	
	log.info("Account Confirmed: {}", aws.getUserDetails)
	aws.uploadToS3(localDeployDir)	//Does a diff
}

object ClusterDeployment extends App with CommonDeployment {
	/*
	 * Assuming a number of EC2 images with the base software (JRE7, R, etc)
	 * have already been spun up.
	 * 
	 * Set:
	 *     -DawsProperties=/mnt/hgfs/share/AWS/aws.properties.txt
	 */
	
	import awsProps._
	
	def runRemoteScript(node: Instance, fileName: String){
		ssh.background(instanceUserName, node.getPublicDnsName(), s"./$fileName")
	}
	
	/*
	 * 
	 * Run Commands
	 * 
	 */
	aws.clusterNodes.foreach{node => 
		ssh.background(instanceUserName, node.getPublicDnsName(), "killall java")
	}

	val master = aws.clusterNodes.head
	val masterPrivateIp = master.getPrivateIpAddress()
	
	aws.clusterNodes.foreach{node => 
		aws.directUpload(s3CfgPath, node)
		
		aws.instanceS3download(node)
		
		val scriptName = createRemoteRunScript(node, masterPrivateIp)
		runRemoteScript(node, scriptName)
		println("started application on "+node.getPrivateIpAddress())
	}
	
	log.info(s"Cluster ready captain.")
	log.info(s"    Connect using ssh -i ${awsProps.sshKeyPath} ec2-user@${master.getPublicDnsName}")
}

object TerminalNodeDeployment extends App with CommonDeployment {
	/*
	 *     -DawsProperties=/mnt/hgfs/share/AWS/aws.properties.txt
	 */
	
	val terminalNode = aws.terminalNode
	
	aws.directUpload(awsProps.s3CfgPath, terminalNode)	
	aws.instanceS3download(terminalNode)
	//TODO undo
	val scriptName = createRemoteRunScript(terminalNode, aws.clusterNodes.head.getPrivateIpAddress())
	
	log.info(s"Terminal ready captain.")
	log.info(s"    Connect using ssh -i ${awsProps.sshKeyPath} ec2-user@${terminalNode.getPublicDnsName}")
	log.info(s"    Run script name: $scriptName")
}

