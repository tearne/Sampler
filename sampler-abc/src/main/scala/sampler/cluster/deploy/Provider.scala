package sampler.cluster.deploy

import org.jclouds.compute.ComputeServiceContext
import org.jclouds.compute.domain.NodeMetadata.Status
import play.api.libs.json._
import scala.collection.JavaConversions._
import scala.util.Try

trait Provider {
  def getAllNodes(): Set[Node]
  val instanceUser: String
}
object Provider {
  val roleTagKey = "role"

  case class NodeInfo(hostname: String, tags: Array[String])
  import play.api.libs.functional.syntax._

  implicit val nodeInfoReads: Reads[NodeInfo] = {
    (JsPath \ "hostname").read[String] and
    (JsPath \ "tags").read[Array[String]]
  }.apply(NodeInfo.apply _)

  def buildJCloudProvider(context: ComputeServiceContext, instanceUser: String): Provider =
    JCloudProvider(context, instanceUser)

  def buildLocalProvider(json: String): LocalProvider = {
    val localProviderRoot = (Json.parse(json) \ "provider" \ "local")
    val nodes = (localProviderRoot \ "nodes")
      .as[Array[NodeInfo]]
      .map(nodeInfo => nodeInfo.hostname -> nodeInfo.tags.toSet)
      .toMap
    val userName = (localProviderRoot \ "instance-user").as[String]

    LocalProvider(nodes, userName)
  }
}

case class JCloudProvider(context: ComputeServiceContext, instanceUser: String) extends Provider {
  val service = context.getComputeService
  def getAllNodes(): Set[Node] = {
    service
      .listNodes
      .map(jCNode => service.getNodeMetadata(jCNode.getId))
      .filter(_.getStatus == Status.RUNNING)
      .map { meta =>
        val tags = meta.getTags.toSet
        val clusternameOpt = tags
          .collect{case tag if tag.startsWith("cluster:") => tag.split(":")(1)}
          .headOption

        val seedRoleOpt = tags
          .collect{case tag if tag.startsWith("seed:") => tag.split(":")(1)}
          .headOption

        Node(
          meta.getHostname,
          Some(Util.getAssertOne(meta.getPublicAddresses.toSet)), //Return None if no IP, or Some('first one')?
          Some(Util.getAssertOne(meta.getPrivateAddresses.toSet)),
          clusternameOpt, //TODO falls over if there is a node without tags!
          seedRoleOpt)
    }.toSet
  }
}

case class LocalProvider(tagsByHostname: Map[String, Set[String]], instanceUser: String) extends Provider {
  def getAllNodes() = {
   tagsByHostname.map { case (hostname, tags) =>
      val clusterNameOpt: Option[String] = tags
        .collect{case tag if tag.startsWith("cluster:") => tag.split(":")(1)}
        .headOption
      val roleOpt: Option[String] = tags
        .collect{case tag if tag.startsWith("seed:") => tag.split(":")(1)}
        .headOption

      Node(
        hostname,
        None,
        Try(java.net.InetAddress.getByName(hostname).getHostAddress).toOption,
        clusterNameOpt,
        roleOpt
      )
    }.toSet
  }
}
