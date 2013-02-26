package sampler.run.cluster.util

import java.nio.file.Paths
import com.amazonaws.auth.PropertiesCredentials
import com.amazonaws.services.ec2.AmazonEC2Client
import java.nio.file.Files
import scala.collection.JavaConversions._
import com.amazonaws.services.identitymanagement.AmazonIdentityManagementClient
import com.amazonaws.services.ec2.model.DescribeInstancesRequest
import com.amazonaws.services.ec2.model.Filter
import com.amazonaws.services.ec2.model.Tag

object AWS{
	private val keyPath = Paths.get("/mnt/hgfs/EC2/AWS-Key.properties")
	private val keyProps = new PropertiesCredentials(Files.newInputStream(keyPath))
	
	val ec2 = new AmazonEC2Client(keyProps);
	ec2.setEndpoint("ec2.eu-west-1.amazonaws.com")
	
	val request = new DescribeInstancesRequest()
	val filter = new Filter("tag:Master", List("true"))
		
	private val instances = AWS.ec2.describeInstances()
		.getReservations
		.map(_.getInstances)
		.flatten
		.filter(_.getState.getName == "running")
		
	val masterPublicName = {
		val candidates = instances.filter(_.getTags().contains(new Tag("master", "true")))
		assert(candidates.size == 1, "Num masters = "+candidates.size)
		candidates(0).getPublicDnsName()
	}
	
	val workersPublicNames = 
		instances.filter(!_.getTags().contains(new Tag("master", "true")))
		.map(_.getPublicDnsName())
		.toList
		
	val allNodeNames = workersPublicNames.+:(masterPublicName)
		
	println(new AmazonIdentityManagementClient(keyProps).getUser)
	println("Master: " + masterPublicName)
	println("Num Workers: " + workersPublicNames.size)
	println(workersPublicNames)
	
	val keyFile = Paths.get("/mnt/hgfs/EC2/otkp.pem")
}