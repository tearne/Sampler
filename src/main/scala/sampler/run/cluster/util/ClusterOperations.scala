package sampler.run.cluster.util

import scala.sys.process.Process
import java.nio.file.{Paths, Path, Files}
import sampler.run.cluster.util.SSHCommand

object ClusterOperations extends App{

	//TODO Chain process
	
	val clusterTag = "b"
	val s3bucket = "ot-bucket"
	val payload = Paths.get("target", "Sampler-assembly-0.0.8.jar")
	val payloadExe = "java -cp Sampler-assembly-0.0.8.jar sampler.run.cluster.WorkerApp"
	
	val s3Path = s"s3://$s3bucket/${payload.getFileName()}"
	
	def directUpload(host: String, file: Path){
		val cmd = s"scp -i ${AWS.keyFile.toString} -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null $file ec2-user@$host:~"
		println(cmd)
		Process(cmd).!
	}
	
	def emptyS3Bucket(bucket: String){
		val command = s"s3cmd del s3://$bucket/*"
		Process(command).!
	}
	
	//TODO gives no feedback in console
	def doS3upload(localPath: Path, s3Target: String){
		val command = s"s3cmd put${if(Files.isDirectory(localPath))" --recursive" else ""} $localPath $s3Target"
		Process(command).!
	}
	
	def resetInstance(host: String){
		val resetInstanceCommand = """killall java; rm -r ~/*"""
		SSHCommand(host, resetInstanceCommand)
	}
	
	def doS3download(host: String, s3Path: String){
		val s3downloadCommand = s"s3cmd get --force --recursive $s3Path"
		SSHCommand(host, s3downloadCommand)
	}
	
	def runPayload(host: String){
		SSHCommand.background(host, payloadExe)
	}
	
	//emptyS3Bucket(s3bucket)
//	doS3upload(payload, s3Path)
//	AWS.clusterNodes(clusterTag).foreach(host => resetInstance(host))
//	AWS.clusterNodes(clusterTag).foreach(host => directUpload(host, Paths.get("/home/user/.s3cfg")))
//	AWS.clusterNodes(clusterTag).foreach(host => doS3download(host, s3Path))
//	AWS.clusterNodes(clusterTag).foreach(host => runPayload(host))
}