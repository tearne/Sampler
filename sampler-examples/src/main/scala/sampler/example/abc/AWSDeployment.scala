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

/*
 * Assuming a number of EC2 images with the base software (JRE7, R, etc)
 * have already been spun up.
 * 
 * Set:
 *     -DawsProperties=/mnt/hgfs/share/AWS/aws.properties.txt
 */
object AWSDeployment extends App with Logging{
	val localDeployDir = Paths.get(System.getProperty("user.home")).resolve("Sampler/sampler-examples/deploy/")
	assert(Files.exists(localDeployDir))	
	log.info("localDeployDir: {}", localDeployDir)
	
	val newLine = System.getProperty("line.separator")
	
	val props = AWSProperties.load
	import props._
	val ssh = new SSH(sshKeyPath)
	val aws = new AWS(props)
	log.info(""+props)
	
	def startApplication(host: String, internalIP: String, masterIP: String) {
		val runCommand = 
s"""
java -Xmx2g \\
-Dakka.remote.netty.tcp.hostname=$internalIP \\
-Dakka.cluster.seed-nodes.0=akka.tcp://ABC@$masterIP:2552 \\
-Dconfig.file=application.conf \\
-Dlogback.statusListenerClass=ch.qos.logback.core.status.OnConsoleStatusListener \\
-Dlogback.configurationFile=logback.xml \\
-cp "lib/*" \\
sampler.example.abc.ClusteredUnfairCoin
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
		
		ssh.scp(instanceUserName, host, tempFile)		
		ssh.background(instanceUserName, host, s"./${tempFile.getFileName()}")
	}
	
	/*
	 * 
	 * Run Commands
	 * 
	 */
	log.info("Confirm account details: {}", aws.getUserDetails)

	aws.clusterNodes.foreach{node => 
		ssh.background(instanceUserName, node.getPublicDnsName(), "killall java")
	}

	aws.uploadToS3(localDeployDir)

	val masterPrivateIp = aws.clusterNodes.head.getPrivateIpAddress()
	
	aws.clusterNodes.foreach{node => 
		val publicDNS = node.getPublicDnsName
		val privateIP = node.getPrivateIpAddress
		
		aws.directUpload(s3CfgPath, node)
		
		aws.instanceS3download(node)
		
		startApplication(publicDNS, privateIP, masterPrivateIp)
		println("started application on "+node.getPrivateIpAddress())
	}
}