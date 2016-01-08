package sampler.cluster.deploy

import org.jclouds.compute.ComputeServiceContext
import org.jclouds.ContextBuilder
import org.jclouds.logging.slf4j.config.SLF4JLoggingModule
import scala.collection.JavaConversions._
import org.jclouds.compute.domain.NodeMetadata.Status
import com.jayway.jsonpath.JsonPath
import scala.util.Try

/*
 * Behind a proxy use:
 * -Djclouds.proxy-host=10.85.4.54 -Djclouds.proxy-port=8080  -Djclouds.proxy-for-sockets=false
 * 
 * or
 * 
 * val overrides = new Properties()
 * overrides.setProperty(Constants.PROPERTY_PROXY_HOST, "10.85.4.54")
 * overrides.setProperty(Constants.PROPERTY_PROXY_PORT, "8080")
 * overrides.setProperty(Constants.PROPERTY_PROXY_FOR_SOCKETS, "false")
 * 
 * and in the contextBuilder add 
 * .overrides(overrides)
 * 
 */

trait Provider {
  def getNodes(): Set[Node]
  val instanceUser: String
}
object Provider {
  val roleTagKey = "role"
}

case class JCloudProvider(context: ComputeServiceContext, instanceUser: String) extends Provider {
  val service = context.getComputeService
  def getNodes(): Set[Node] = {
    service
      .listNodes
      .map(jCNode => service.getNodeMetadata(jCNode.getId))
      .filter(_.getStatus == Status.RUNNING)
      .map { meta =>
        val userMeta = meta.getUserMetadata
        
        Node(
          meta.getHostname,
          Some(Util.getAssertOne(meta.getPublicAddresses.toSet)), //Return None if no IP, or Some('first one')?
          Some(Util.getAssertOne(meta.getPrivateAddresses.toSet)),
          //Some(userMeta.get(Provider.roleTagKey)))  //TODO what if we forget to tag a node?
          Some(meta.getTags.head))
      }.toSet
  }
}

case class LocalProvider(nodeRolesByHostname: Map[String, String], instanceUser: String) extends Provider {
  def getNodes(): Set[Node] = {
    nodeRolesByHostname.map { case (hostname, role) => 
      Node(
          hostname,
          None, 
          Try(java.net.InetAddress.getByName(hostname).getHostAddress).toOption, 
          Some(role)) 
    }.toSet
  }

}
object LocalProvider {
  def buildProvider(json: String): LocalProvider = {
    val readJson = JsonPath.parse(json)
    import scala.collection.JavaConversions._
    import java.util.{List => JavaList}
    import java.util.{Map => JavaMap}
    val nodes = readJson
      .read[JavaList[JavaMap[String, String]]]("$.provider.local.nodes")
      .map(_.toMap)
      .map(m => m("hostname") -> m("role"))
      .toMap

    LocalProvider(nodes, readJson.read[String]("$.provider.local.instance-user"))
  }
}

object CloudSigma {
  def buildProvider(json: String): Provider = {
    val readJson = JsonPath.parse(json)

    lazy val context = {
      import scala.collection.JavaConversions._
      ContextBuilder
        .newBuilder("cloudsigma2")
        .credentials(
          readJson.read[String]("$.provider.cloud-sigma.api.user"),
          readJson.read[String]("$.provider.cloud-sigma.api.pass"))
        .modules(Set(new SLF4JLoggingModule()))
        .buildView(classOf[ComputeServiceContext])
    }

    JCloudProvider(
      context,
      readJson.read[String]("$.provider.cloud-sigma.instance-user"))
  }
}

object AWS {
  def buildProvider(json: String): Provider = {
    val readJson = JsonPath.parse(json)

    val context = {
      import scala.collection.JavaConversions._

      ContextBuilder
        .newBuilder("aws-ec2")
        .credentials(
          readJson.read[String]("$.provider.aws-ec2.api.access-key"),
          readJson.read[String]("$.provider.aws-ec2.api.secret-key"))
        .modules(Set(new SLF4JLoggingModule()))
        .buildView(classOf[ComputeServiceContext])
    }
    JCloudProvider(
      context,
      readJson.read[String]("$.provider.aws-ec2.instance-user"))
  }
}

object SoftLayer {
  def buildProvider(json: String): Provider = {
    val readJson = JsonPath.parse(json)

    val context = {
      import scala.collection.JavaConversions._

      ContextBuilder
        .newBuilder("softlayer")
        .credentials(
          readJson.read[String]("$.provider.softlayer.api.user"),
          readJson.read[String]("$.provider.softlayer.api.pass"))
        .modules(Set(new SLF4JLoggingModule()))
        .buildView(classOf[ComputeServiceContext])
    }
    JCloudProvider(
      context,
      readJson.read[String]("$.provider.softlayer.instance-user"))
  }
}