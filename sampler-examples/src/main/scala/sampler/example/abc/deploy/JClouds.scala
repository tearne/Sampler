package sampler.example.abc.deploy

import sampler.cluster.deploy.Provider
import play.api.libs.json._
import org.jclouds.ContextBuilder

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

object CloudSigma {
  def buildProvider(jsonStr: String): Provider = {
    // val readJson = JsonPath.parse(json)
    val json = Json.parse(jsonStr)


    lazy val context = {
      // import scala.collection.JavaConversions._
      ContextBuilder
        .newBuilder("cloudsigma2")
        .credentials(
          (json \ "provider" \ "cloud-sigma" \ "api.user").as[String],
          (json \ "provider" \ "cloud-sigma" \ "api.pass").as[String])
        .modules(Set(new SLF4JLoggingModule()))
        .buildView(classOf[ComputeServiceContext])
    }

    Provider.buildJCloudProvider(
      context,
      (json \ "provider" \ "cloud-sigma" \ "instance-user").as[String]
    )
  }
}

object AWS {
  def buildProvider(jsonStr: String): Provider = {
    // val readJson = JsonPath.parse(json)
    val json = Json.parse(jsonStr)

    val context = {
      // import scala.collection.JavaConversions._

      ContextBuilder
        .newBuilder("aws-ec2")
        .credentials(
          (json \ "provider" \ "aws-ec2" \ "api.access-key").as[String],
          (json \ "provider" \ "aws-ec2" \ "api.secret-key").as[String])
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
      readJson.read[String]("$.provider.softlayer.instance-user")
      )
  }
}
