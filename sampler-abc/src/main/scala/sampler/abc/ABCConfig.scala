package sampler.abc

import com.typesafe.config.{Config, ConfigRenderOptions}

import scala.concurrent.duration.MILLISECONDS

case class ABCConfig(config: Config) {
  lazy val job = config.getConfig("abc.job")
  lazy val algorithm = config.getConfig("abc.algorithm")
  lazy val cluster = config.getConfig("abc.cluster")
  
  def renderJob(): String = job.root.render(ConfigRenderOptions.concise.setOriginComments(false))
  def renderAlgorithm(): String = algorithm.root.render(ConfigRenderOptions.concise.setOriginComments(false))
  def renderCluster(): String = cluster.root.render(ConfigRenderOptions.concise.setOriginComments(false))

  // Use of lazy allows easy overriding in tests
  lazy val numReplicates =  job.getInt("replicates")
  lazy val numParticles =   job.getInt("particles")
  lazy val numGenerations = job.getInt("generations")
  
  lazy val particleChunkSize =  algorithm.getInt("particle-chunk-size")
  lazy val maxParticleRetries = algorithm.getInt("particle-retries")
  lazy val toleranceDescentPercentile =  algorithm.getDouble("tolerance-descent-percentile")
  lazy val minNumLocalParticles = algorithm.getInt("fewest-accepted-local-particles")

  lazy val clusterName =          cluster.getString("system-name")
  lazy val mixRateMS: Long =      cluster.getDuration("mixing.rate", MILLISECONDS)
  lazy val mixResponseTimeoutMS = cluster.getDuration("mixing.response-threshold", MILLISECONDS)
  lazy val mixPayloadSize =       cluster.getInt("mixing.num-particles")
  lazy val clusterSizeReportMS =  cluster.getDuration("size-reporting", MILLISECONDS)
  lazy val terminateAtTargetGen = cluster.getBoolean("terminate-at-target-generation")
  lazy val maxParticleMemory =    cluster.getInt("max-particle-memory")
  lazy val futuresTimeoutMS =     cluster.getDuration("futures-timeout", MILLISECONDS)
}