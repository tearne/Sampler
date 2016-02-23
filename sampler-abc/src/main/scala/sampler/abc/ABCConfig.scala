package sampler.abc

import com.typesafe.config.Config
import scala.concurrent.duration.MILLISECONDS
import com.typesafe.config.ConfigRenderOptions

case class ABCConfig(config: Config) {
  def render: String = config.root.render(ConfigRenderOptions.defaults.setOriginComments(false))
  
  val job = config.getConfig("abc.job")
  val algorithm = config.getConfig("abc.algorithm")
  val cluster = config.getConfig("abc.cluster")
  
  //TODO Do these need to be lazy for the tests to work?
  lazy val numReplicates =  job.getInt("replicates")
  lazy val numParticles =   job.getInt("particles")
  lazy val numGenerations = job.getInt("generations")
  
  lazy val particleChunkSize =  algorithm.getInt("particle-chunk-size")
  lazy val maxParticleRetries = algorithm.getInt("particle-retries")

  lazy val mixRateMS: Long =      cluster.getDuration("mixing.rate", MILLISECONDS)
  lazy val mixResponseTimeoutMS = cluster.getDuration("mixing.response-threshold", MILLISECONDS)
  lazy val mixPayloadSize =       cluster.getInt("mixing.num-particles")
  lazy val clusterSizeReportMS =  cluster.getDuration("size-reporting", MILLISECONDS)
  lazy val terminateAtTargetGen = cluster.getBoolean("terminate-at-target-generation")
  lazy val memoryGenerations =    cluster.getInt("particle-memory-generations")
  lazy val futuresTimeoutMS =     cluster.getDuration("futures-timeout", MILLISECONDS)
}