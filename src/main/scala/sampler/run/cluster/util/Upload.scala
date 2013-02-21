package sampler.run.cluster.util

import java.nio.file.Path
import scala.sys.process.Process
import scala.collection.JavaConversions._
import java.nio.file.{Paths,Files}
import com.amazonaws.services.ec2.AmazonEC2Client
import com.amazonaws.auth.PropertiesCredentials
import com.amazonaws.services.ec2.model.DescribeInstancesRequest
import com.amazonaws.services.identitymanagement.AmazonIdentityManagementClient
import java.nio.charset.Charset

object Uploader extends App{
	val data = Paths.get("/mnt/hgfs/EC2/upload")
	
	def upload(file: Path, host: String){
		def rsyncArgs() = {
			List(
					"-az",
					"-v",
					"--progress",
					"-e",
					"ssh -i %s -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null".format(AWS.keyFile.toString),
					file.toString,
					"ec2-user@%s:/srv".format(host.toString)
			)
		}
	
		val t = Process("rsync", rsyncArgs).!
		println(t)
	}
	
	upload(data, AWS.masterPublicName)
}