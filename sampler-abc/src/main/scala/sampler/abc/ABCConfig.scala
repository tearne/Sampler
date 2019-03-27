package sampler.abc

import com.typesafe.config.{Config, ConfigRenderOptions}
import play.api.libs.json.Json

import scala.concurrent.duration.MILLISECONDS

case class ABCConfig(config: Config) {
  lazy val asJson = Json.parse(config.root().render(ConfigRenderOptions.concise()))

  lazy val job: Config = config.getConfig("abc.job")
  lazy val algorithm: Config = config.getConfig("abc.algorithm")
  lazy val cluster: Config = config.getConfig("abc.cluster")

  def renderJob(): String = job.root.render(ConfigRenderOptions.concise.setOriginComments(false))

  def renderAlgorithm(): String = algorithm.root.render(ConfigRenderOptions.concise.setOriginComments(false))

  def renderCluster(): String = cluster.root.render(ConfigRenderOptions.concise.setOriginComments(false))

  // Use of lazy allows easy overriding in tests
  lazy val numReplicates: Int = job.getInt("replicates")
  lazy val numParticles: Int = job.getInt("particles")
  lazy val numGenerations: Int = job.getInt("generations")

  lazy val particleChunkSize: Int = algorithm.getInt("particle-chunk-size")
  lazy val maxParticleRetries: Int = algorithm.getInt("particle-retries")
  lazy val toleranceDescentPercentile: Double = algorithm.getDouble("tolerance-descent-percentile")
  lazy val minNumLocalParticles: Int = algorithm.getInt("fewest-accepted-local-particles")

  lazy val clusterName: String = cluster.getString("system-name")
  lazy val mixRateMS: Long = cluster.getDuration("mixing.rate", MILLISECONDS)
  lazy val mixResponseTimeoutMS: Long = cluster.getDuration("mixing.response-threshold", MILLISECONDS)
  lazy val mixPayloadSize: Int = cluster.getInt("mixing.num-particles")
  lazy val clusterSizeReportMS: Long = cluster.getDuration("size-reporting", MILLISECONDS)
  lazy val terminateAtTargetGen: Boolean = cluster.getBoolean("terminate-at-target-generation")
  lazy val maxParticleMemory: Int = cluster.getInt("max-particle-memory")
  lazy val futuresTimeoutMS: Long = cluster.getDuration("futures-timeout", MILLISECONDS)
}