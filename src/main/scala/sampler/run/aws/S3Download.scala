package sampler.run.aws
import scala.collection.JavaConversions._
import java.nio.file.{Paths,Files}
import com.amazonaws.auth.PropertiesCredentials
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.GetObjectRequest
import com.amazonaws.regions.Region
import com.amazonaws.regions.Regions;

object S3Download extends App{
	val wd = Paths.get("/mnt/hgfs/EC2/upload")
	private val keyPath = Paths.get("/mnt/hgfs/EC2/AWS-Key.properties")
	private val keyProps = new PropertiesCredentials(Files.newInputStream(keyPath))
	val s3 = new AmazonS3Client(keyProps)
	s3.setRegion(Region.getRegion(Regions.EU_WEST_1))
	
	val bucketName = "ot-bucket"
	val key = "Rowan.jpg"
		
	val file = wd.resolve(key).toFile()
	val s3Obj = s3.getObject(new GetObjectRequest(bucketName, key), file)
}