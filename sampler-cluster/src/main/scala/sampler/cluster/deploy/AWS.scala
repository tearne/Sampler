package sampler.cluster.deploy

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Properties

import scala.collection.JavaConversions.asScalaBuffer
import scala.sys.process.Process
import scala.sys.process.ProcessLogger

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
	clusterFilter: Tag,
	terminalFilter: Tag,
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
			new Tag(props.getProperty("tagName"),props.getProperty("clusterTag")),
			new Tag(props.getProperty("tagName"),props.getProperty("terminalTag")),
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
		
	def clusterNodes = {
		val nodes = runningInstances
			.filter(_.getTags().contains(props.clusterFilter))
			.toList
		if(nodes.size == 0) log.warn("No instances running")
		nodes
	}
	
	def terminalNode = {
		val nodes = runningInstances
			.filter(_.getTags().contains(props.terminalFilter))
			.toList
		assert(nodes == 1, s"Expected precisely one terminal node, found ${nodes.size}")
		nodes.head
	}
			
	def directUpload(path: Path, node: Instance){
		import props._
		ssh.scp(instanceUserName, node.getPublicDnsName(), path)
	}
			
	def uploadToS3(localDir: Path){
		assert(Files.isDirectory(localDir))
		val commandSeq = Seq[String](
				"s3cmd",
				"sync",
				"-c",
				props.s3CfgPath.toString,
				"--delete-removed",
				"--progress",
				"--recursive",
				localDir.toString+"/",
				props.s3Bucket
		)
		log.info("Command: {}", commandSeq.map(_+" ").mkString)
		Process(commandSeq) ! ProcessLogger(line => log.info(line))
	}
	
	def instanceS3download(instance: Instance) {
		val command = s"s3cmd sync ${props.s3Bucket} ."
		ssh.forground(props.instanceUserName, instance.getPublicDnsName(), command)
		log.info("done s3download")
	}
}