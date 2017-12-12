package sampler.abc

import com.typesafe.config.ConfigFactory
import org.scalatest.FreeSpec

import scala.concurrent.duration._

class ABCConfigTest extends FreeSpec {
  val fullConfig = ConfigFactory.parseString(
    """
    my-test-config.abc {
      job {
        replicates = 1000
        particles = 2000
        generations = 50
      }
      algorithm {
        particle-retries = 100
        particle-chunk-size = 200
        tolerance-descent-percentile = 0.7
        min-num-local-particles = 5
      }
      cluster {
        system-name = "MyCluster"
        max-particle-memory = 10000
        terminate-at-target-generation = true
        futures-timeout = 10 hour
        mixing {
          rate = 500 milliseconds
          num-particles = 150
          response-threshold = 400 milliseconds
        }
      }
    }
  """)

  val instance = ABCConfig(fullConfig.getConfig("my-test-config"))
  val altInstance = ABCConfig(
    ConfigFactory.parseString(
      "my-test-config.abc.cluster.terminate-at-target-generation = false"
    )
        .withFallback(fullConfig)
        .getConfig("my-test-config")
  )

  "Should parse" - {
    "Algorithm parameters" in {
      assertResult(100)(instance.maxParticleRetries)
      assertResult(200)(instance.particleChunkSize)
      assertResult(0.7)(instance.toleranceDescentPercentile)
      assertResult(5)(instance.minNumLocalParticles)
    }
    "Job parameters" in {
      assertResult(1000)(instance.numReplicates)
      assertResult(2000)(instance.numParticles)
      assertResult(50)(instance.numGenerations)
    }
    "Cluster parameters" in {
      assertResult("MyCluster")(instance.clusterName)
      assertResult(10000)(instance.maxParticleMemory)
      assert(instance.terminateAtTargetGen)
      assertResult(10.hour.toMillis)(instance.futuresTimeoutMS)
      assertResult(500)(instance.mixRateMS)
      assertResult(150)(instance.mixPayloadSize)
      assertResult(400)(instance.mixResponseTimeoutMS)

      assertResult(false)(altInstance.terminateAtTargetGen)
    }
  }
}