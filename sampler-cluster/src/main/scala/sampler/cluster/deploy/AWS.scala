package sampler.cluster.deploy

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Properties

import scala.collection.JavaConversions.asScalaBuffer
import scala.sys.process._
//import scala.sys.process.Process._
//import scala.sys.process.ProcessLogger

import com.amazonaws.auth.AWSCredentials
import com.amazonaws.services.ec2.AmazonEC2Client
import com.amazonaws.services.ec2.model.Instance
import com.amazonaws.services.ec2.model.Tag
import com.amazonaws.services.identitymanagement.AmazonIdentityManagementClient

import sampler.io.Logging

object AWSTest extends App{
	println(AWSProperties.load)
}

case class AWSProperties(
	accessKey: String,
	secretKey: String,
	endpoint: String, 
	masterTag: Tag,
	workerTag: Tag,
	instanceUserName: String,
	s3Bucket: String,
	s3CfgPath: Path,
	sshKeyPath: Path
) extends AWSCredentials {
	def getAWSAccessKeyId = accessKey
	def getAWSSecretKey = secretKey
}

object AWSProperties {
	def load = {
		val props = new Properties()
		val location = System.getProperty("awsProperties")
		assert(location != null, "'awsProperties' not found")
		val propertiesPath = Paths.get(location)
		assert(Files.exists(propertiesPath), s"propertiesPath does not exist")
		props.load(Files.newInputStream(propertiesPath))
		AWSProperties(
			props.getProperty("accessKey"),
			props.getProperty("secretKey"),
			props.getProperty("endpoint"),
			new Tag(props.getProperty("tagName"),props.getProperty("masterTag")),
			new Tag(props.getProperty("tagName"),props.getProperty("workerTag")),
			props.getProperty("instanceUserName"),
			props.getProperty("s3Bucket"),
			resolve(propertiesPath, props.getProperty("s3cfgPath")),
			resolve(propertiesPath, props.getProperty("sshKeyPath"))
		)
	}
	
	private def resolve(propertiesPath: Path, targetPathStr: String): Path= {
		val p = Paths.get(targetPathStr)
		val result = if(p.isAbsolute()) p
		else propertiesPath.getParent().resolve(p)
		assert(Files.exists(result), s"$result does not exist")
		result
	}
}

class AWS(props: AWSProperties) extends Logging{
	val ec2 = new AmazonEC2Client(props)
	ec2.setEndpoint(props.endpoint)
	
	val ssh = new SSH(props.sshKeyPath)
	
	def getUserDetails = new AmazonIdentityManagementClient(props).getUser
	
	def runningInstances = ec2
			.describeInstances()
			.getReservations
			.map(_.getInstances)
			.flatten
			.filter(_.getState.getName == "running")
		
	
	def masterNode = {
		val nodes = runningInstances
			.filter(_.getTags().contains(props.masterTag))
			.toList
		assert(nodes.size == 1, s"Expected precisely one master node, found ${nodes.size}")
		nodes.head
	}
			
	def workerNodes = {
		val nodes = runningInstances
			.filter(_.getTags().contains(props.workerTag))
			.toList
		if(nodes.size == 0) log.warn("No instances running")
		nodes
	}
	
	def scpUpload(localPath: Path, node: Instance, remoteDestination: String){
		val cmd = ssh.scpCommand(props.instanceUserName, node.getPublicDnsName(), localPath, remoteDestination)
		log.info(cmd)
		Process(cmd) ! ProcessLogger(line => log.info(line))
	}
			
	def s3Upload(localDir: Path){
		assert(Files.isDirectory(localDir))
		val cmd = Seq[String](
				"s3cmd",
				"sync",
				"-c",
				props.s3CfgPath.toString,
				"--delete-removed",
				"--progress",
				"--recursive",
				localDir.toString+"/",
				props.s3Bucket
		).mkString(" ")
		log.info(cmd)
		Process(cmd) ! ProcessLogger(line => log.info(line))
	}
	
	def s3RemoteDownload(instance: Instance, remoteDir: String) {
		val dirTrailingSlash = if(!remoteDir.endsWith("/")) remoteDir+"/" else remoteDir
		val command = s"mkdir -p $dirTrailingSlash && s3cmd sync --delete-removed ${props.s3Bucket}/ $dirTrailingSlash"
		val fullCmd = ssh.forgroundCommand(props.instanceUserName, instance.getPublicDnsName(), command)
		log.info(fullCmd)
		Process(fullCmd) ! ProcessLogger(line => log.debug(line))
	}
}