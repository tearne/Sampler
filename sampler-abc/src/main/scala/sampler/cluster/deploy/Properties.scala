package sampler.cluster.deploy

import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.Paths
import scala.util.Try
import play.api.libs.json._

case class Properties(
  privateKeyPath: Path,
  payloadLocal: Path,
  payloadTarget: String,
  applicationMain: String,
  vmExtraArgs: Seq[String]
){
  val payloadTargetParent = payloadTarget
      .take(payloadTarget.lastIndexOf('/'))
}

object Properties extends App {
  def pathHelper(path: String): Path = Paths.get(
    path.replaceFirst("^~", System.getProperty("user.home"))
  )

  def fromJSON(jsonStr: String) = {
    val json = Json.parse(jsonStr)

    val privateKeyPath = pathHelper((json \ "ssh-key").as[String])
    val payloadLocal =  pathHelper((json \ "payload" \ "local-dir").as[String])

    assume(
      Files.exists(privateKeyPath),
      "Private SSH key file not found: "
        +s"${privateKeyPath.toAbsolutePath}")
    assume(
      Files.exists(payloadLocal),
      "Local deploy directory does not exist: "
        +s"${payloadLocal.toAbsolutePath}")

    // import scala.collection.JavaConversions._

    Properties(
      privateKeyPath,
      payloadLocal,
      (json \ "payload" \ "remote-target").as[String],
      (json \ "jvm" \ "application-main").as[String],
      (json \ "jvm" \ "extra-args").as[Seq[String]]
    )
  }
}
