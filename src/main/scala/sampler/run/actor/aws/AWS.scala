package sampler.run.actor.aws

import java.nio.file.Paths
import com.amazonaws.auth.PropertiesCredentials
import com.amazonaws.services.ec2.AmazonEC2Client
import java.nio.file.Files
import scala.collection.JavaConversions._
import com.amazonaws.services.identitymanagement.AmazonIdentityManagementClient
import com.amazonaws.services.ec2.model.DescribeInstancesRequest
import com.amazonaws.services.ec2.model.Filter
import com.amazonaws.services.ec2.model.Tag

object Test extends App{
	AWS
}

object AWS{
	private val keyPath = Paths.get("/mnt/hgfs/EC2/AWS-Key.properties")
	private val keyProps = new PropertiesCredentials(Files.newInputStream(keyPath))
	
	val ec2 = new AmazonEC2Client(keyProps);
	ec2.setEndpoint("ec2.eu-west-1.amazonaws.com")
	
	val request = new DescribeInstancesRequest()
	val filter = new Filter("tag:Master", List("true"))
		
	def runningInstances = AWS.ec2.describeInstances()
		.getReservations
		.map(_.getInstances)
		.flatten
		.filter(_.getState.getName == "running")
		
	def clusterNodes(tagName: String, tagValue: String) = {
		runningInstances.filter(_.getTags().contains(new Tag(tagName, tagValue)))
			.map(_.getPublicDnsName())
			.toList
	}
	
	println(new AmazonIdentityManagementClient(keyProps).getUser)
	runningInstances.view.map(_.getPrivateIpAddress()).foreach(println)
	
	val keyFile = Paths.get("/mnt/hgfs/EC2/otkp.pem")
}