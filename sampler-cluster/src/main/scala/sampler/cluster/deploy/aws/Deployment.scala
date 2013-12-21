package sampler.cluster.deploy.aws

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


/*
 * Assuming that images have been created from an AMI which 
 * has the base software pre-installed
 */

object Deployment extends App with Logging{
	val localDeployDir = Paths.get(System.getProperty("user.home")).resolve("Sampler/sampler-examples/deploy")
	log.info("localDeployDir: {}", localDeployDir)
	
	val targetTag = new Tag("Name", "test")
	val s3DeployBucket = "s3://ot-test1"	
	
	val credentialsPath = Paths.get("/mnt/hgfs/share/AWS/")
	val accessKeyPath = credentialsPath.resolve("accessKey.properties")
	val sshPemPath = credentialsPath.resolve("otkp.pem")
	val s3cmdCfgPath = credentialsPath.resolve(".s3cfg")
	
	assert(Files.exists(localDeployDir))	
	assert(Files.exists(accessKeyPath))
	assert(Files.exists(sshPemPath))
	assert(Files.exists(s3cmdCfgPath))
	
	val newLine = System.getProperty("line.separator")
	
	val endpoint = "ec2.eu-west-1.amazonaws.com"

	val user = "ec2-user"
	
	val keyProps = new PropertiesCredentials(Files.newInputStream(accessKeyPath))
	val ec2 = new AmazonEC2Client(keyProps); ec2.setEndpoint(endpoint)
	
	val ssh = new SSH(Some(sshPemPath))
	
	def userStuff = new AmazonIdentityManagementClient(keyProps).getUser
	
	def runningInstances = ec2
		.describeInstances()
		.getReservations
		.map(_.getInstances)
		.flatten
		.filter(_.getState.getName == "running")
		
	def clusterNodes = runningInstances
		.filter(_.getTags().contains(targetTag))
		.toList
		
	def emptyS3Bucket{
		val command = s"s3cmd del $s3DeployBucket/*"
		log.info("Local command: {}", command)
		//TODO do ssh commands like this
		command ! ProcessLogger(line => log.info(line))
	}
	
	def startApplication(host: String, internalIP: String, masterIP: String) {
		val runCommand = 
s"""
java -Xmx3g \\
-Dakka.remote.netty.tcp.hostname=$internalIP \\
-Dakka.cluster.seed-nodes.0=akka.tcp://ABC@$masterIP:2552 \\
-Dconfig.file=application.conf \\
-Dlogback.statusListenerClass=ch.qos.logback.core.status.OnConsoleStatusListener \\
-Dlogback.configurationFile=logback.xml \\
-cp "lib/*" \\
sampler.example.abc.ClusteredUnfairCoin
"""

//		val tempDir = Paths.get(System.getProperty("java.io.tmpdir"))
		val tempFile = Files.createTempFile("run", ".sh")
		tempFile.toFile().deleteOnExit
		log.info("Temp file path is {}", tempFile)
//Delete the temporary directory
//            temp.toFile().deleteOnExit();
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
		
		ssh.scp(user, host, tempFile)		
		ssh.background(user, host, s"./${tempFile.getFileName()}")
	}
	
	def stopApplication(host: String) {
		ssh.background(user, host, "killall java")
	}
	
	def transferS3CmdConfig(host: String){
		ssh.scp(user, host, s3cmdCfgPath)
	}
	
	//TODO gives no feedback in console
	def doS3upload(localPath: Path, s3Target: String){
		val command = s"s3cmd sync --delete-removed --progress${if(Files.isDirectory(localPath))" --recursive" else ""} $localPath/ $s3Target/"
//		val command = "find /etc"
		log.info("Local command: {}", command)
		command ! ProcessLogger(line => log.info(line))
	}
	
	def instanceS3download(host: String) {
		val s3InstanceDownload = s"s3cmd sync $s3DeployBucket ."
		ssh.forground(user, host, s3InstanceDownload)
		log.info("done s3download")
	}
		
	/*
	 * 
	 * Run Commands
	 * 
	 */
	log.info("Confirm account details: {}", userStuff)

	clusterNodes.foreach{node => 
		stopApplication(node.getPublicDnsName())
	}

//	emptyS3Bucket
	doS3upload(localDeployDir, s3DeployBucket)

	val masterPrivateIp = clusterNodes.head.getPrivateIpAddress()
	clusterNodes.foreach{node => 
		val publicDNS = node.getPublicDnsName
		val privateIP = node.getPrivateIpAddress
		
		transferS3CmdConfig(publicDNS)
		
		//TODO only do this if not already there?
		// or sync is enough?
		instanceS3download(publicDNS)
		
		startApplication(publicDNS, privateIP, masterPrivateIp)
		println("started application on "+node.getPrivateIpAddress())
	}
//	
}