package sampler.example.abc.deploy

import sampler.cluster.deploy.Provider
import play.api.libs.json._
import org.jclouds.ContextBuilder
import org.jclouds.logging.slf4j.config.SLF4JLoggingModule
import org.jclouds.compute.ComputeServiceContext
import sampler.cluster.deploy.JCloudProvider

/*
 * Behind a proxy use:
 * -Djclouds.proxy-host=[IP] -Djclouds.proxy-port=[port]  -Djclouds.proxy-for-sockets=false
 *
 * or
 *
 * val overrides = new Properties()
 * overrides.setProperty(Constants.PROPERTY_PROXY_HOST, "[IP]")
 * overrides.setProperty(Constants.PROPERTY_PROXY_PORT, "[Port]")
 * overrides.setProperty(Constants.PROPERTY_PROXY_FOR_SOCKETS, "false")
 *
 * and in the contextBuilder add
 * .overrides(overrides)
 *
 */

object AWS {
  def buildProvider(jsonStr: String): Provider = {
    val json = Json.parse(jsonStr)

    val context = {
      import scala.collection.JavaConversions._

      ContextBuilder
        .newBuilder("aws-ec2")
        .credentials(
          (json \ "provider" \ "aws-ec2" \ "api.access-key").as[String],
          (json \ "provider" \ "aws-ec2" \ "api.secret-key").as[String])
        .modules(Set(new SLF4JLoggingModule()))
        .buildView(classOf[ComputeServiceContext])
    }
    Provider.buildJCloudProvider(
      context,
      (json \ "provider" \ "aws-ec2" \ "instance-user").as[String]
    )
  }
}

object SoftLayer {
  def buildProvider(jsonStr: String): Provider = {
    val json = Json.parse(jsonStr)

    val context = {
      import scala.collection.JavaConversions._

      ContextBuilder
        .newBuilder("softlayer")
        .credentials(
          (json \ "provider" \ "softlayer" \ "api" \ "user").as[String],
          (json \ "provider" \ "softlayer" \ "api" \ "pass").as[String])
        .modules(Set(new SLF4JLoggingModule()))
        .buildView(classOf[ComputeServiceContext])
    }
    Provider.buildJCloudProvider(
      context,
      (json \ "provider" \ "softlayer" \ "instance-user").as[String]
    )
  }
}
