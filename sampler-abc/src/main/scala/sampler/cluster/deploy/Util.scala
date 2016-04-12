package sampler.cluster.deploy

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermission
import java.nio.charset.Charset
import scala.collection.JavaConversions

object Util {
  def getAssertOne[T](set: Set[T]) = {
    assume(set.size == 1, s"Expected one item but got: $set")
    set.head
  }
  
  def writeToTempFile(content: String, prefix: String, suffix: String): Path = {
  	val path = Files.createTempFile(prefix, suffix)
		path.toFile().deleteOnExit
		
		import scala.collection.JavaConversions._
		Files.setPosixFilePermissions(path, Set(
				PosixFilePermission.OWNER_EXECUTE,
				PosixFilePermission.OWNER_READ,
				PosixFilePermission.OWNER_WRITE,
				PosixFilePermission.GROUP_EXECUTE,
				PosixFilePermission.OTHERS_EXECUTE
		))
		
		val writer = Files.newBufferedWriter(path, Charset.defaultCharset())
		writer.write(content)
		writer.close
		
		path
  }
}