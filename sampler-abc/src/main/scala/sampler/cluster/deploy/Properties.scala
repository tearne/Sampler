package sampler.cluster.deploy

import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.Paths
import com.jayway.jsonpath.JsonPath
import scala.util.Try

case class Properties(
  privateKeyPath: Path,
  payloadLocal: Path,
  tearDown: Boolean,
  payloadTarget: String,
  applicationMain: String,
  vmExtraArgs: Seq[String]
){
  val payloadTargetParent = payloadTarget.take(payloadTarget.lastIndexOf('/'))
}

object Properties extends App {
  def pathHelper(path: String): Path = Paths.get(path.replaceFirst("^~", System.getProperty("user.home")))
  
  def fromJSON(json: String) = {
    val readContext = JsonPath.parse(json)
    
    val privateKeyPath = pathHelper(readContext.read[String]("$.ssh-key"))
    val payloadLocal = pathHelper(readContext.read[String]("$.payload.local-dir"))
    val tearDown = Try(readContext.read[String]("$.teardown").toBoolean).getOrElse(false)    
    
    assume(Files.exists(privateKeyPath), s"Private SSH key file not found: ${privateKeyPath.toAbsolutePath}")
    assume(Files.exists(payloadLocal), s"Local deploy directory does not exist: ${payloadLocal.toAbsolutePath}")
    
    import scala.collection.JavaConversions._
    
    //val args = readContext.read[java.util.List[String]]("$.jvm.extra-args")
    //println(args)
    
    Properties(
      privateKeyPath,
      payloadLocal,
      tearDown,
      readContext.read[String]("$.payload.remote-target"),
      readContext.read("$.jvm.application-main"),
      readContext.read[java.util.List[String]]("$.jvm.extra-args")
    )
  }
}