package sampler.cluster.abc.actor

import sampler.cluster.abc.config.ABCConfig._
import sampler.cluster.abc.config.ClusterParameters._
import sampler.cluster.abc.config.JobParameters._

trait Lenses {
	val mixRateMS = clusterLens >=> mixRateMSLens
	val numParticles = jobLens >=> numParticlesLens
}