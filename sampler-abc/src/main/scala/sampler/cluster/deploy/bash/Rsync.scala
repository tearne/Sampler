package sampler.cluster.deploy.bash

import java.nio.file.Path
import scala.collection.JavaConversions._
import java.nio.file.Paths

object RsyncTest extends App {
  println(new Rsync(Paths.get("")).apply("me", "host", Paths.get("src"), "dest").mkString(" "))
}

class Rsync(keyFile: Path){
	def apply(userName: String, host: String, src: Path, dest: String): List[String] = {
		List(
				"rsync",
		    "-az",
		    "--delete",
				"-v",
				"--progress",
				"-e",
				"ssh -i %s -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null".format(keyFile.toAbsolutePath.toString),
				src.toAbsolutePath.toString,
				s"$userName@$host:$dest"
		)
	}
}