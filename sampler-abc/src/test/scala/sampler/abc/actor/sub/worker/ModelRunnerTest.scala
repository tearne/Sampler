package sampler.abc.actor.sub.worker

import org.scalatest.FreeSpec
import org.scalatest.mock.MockitoSugar
import sampler.abc.Model
import sampler.abc.Generation
import sampler.abc.Population
import sampler.abc.config.ABCConfig
import sampler.abc.actor.sub.GenerateParticlesFrom
import sampler.abc.config.JobParameters
import sampler.abc.config.ClusterParameters
import sampler.abc.config.AlgorithmParameters
import scala.util.Try
import sampler.abc.actor.main.ScoredParticles
import sampler.abc.Prior
import org.mockito.Mockito.when
import sampler.abc.Scored
import scala.util.Success


class ModelRunnerTest extends FreeSpec with MockitoSugar {
  type T = Int //Pretend model parameters
  val noMixing = 0l
  val hundredParticles = 100
  val threeGenerations = 3
  val isFinal = true
  val terminateAtTargetGen = true //TODO false
  val maxParticleRetries = 3
  val particleChunkSize = 1
  
  class TestableModelRunner(
		val config: ABCConfig)
			extends ModelRunnerComponent[T]
      with AborterComponent {
    //	self: AborterComponent =>
		val random = sampler.math.Random
		val model: Model[T] = mock[Model[T]]
		val modelRunner = new ModelRunner {}
    val aborter = new Aborter {}
	}

  trait Setup {
    val prevPopulation: Generation[T] = Population(Map(1 -> 0.2, 2 -> 0.8), 0, 0)
    val config1 = ABCConfig(
      JobParameters(hundredParticles, 0, threeGenerations),
      AlgorithmParameters(maxParticleRetries,particleChunkSize),
      ClusterParameters(terminateAtTargetGen, 0, 0l, 0, noMixing, 0l, 0l))
      
    val instance1 = new TestableModelRunner(config1)
    val job1 = GenerateParticlesFrom[T](prevPopulation, config1)
    
    // Second config,instance and job is where maxParticleRetries is 0 to trigger exception
    val config2 = ABCConfig(
      JobParameters(hundredParticles, 0, threeGenerations),
      AlgorithmParameters(0,particleChunkSize),
      ClusterParameters(terminateAtTargetGen, 0, 0l, 0, noMixing, 0l, 0l))
    val instance2 = new TestableModelRunner(config2)
    val job2 = GenerateParticlesFrom[T](prevPopulation, config2)
  }
     
  "ModelRunner should / " - {
		"Return scored parameters when given proper proposal distribution" in new Setup {
		  val expected = Success(Scored(1, Seq(0.4, 0.49, 0.5, 0.51))) // 2/4 under threshold
		  assertResult(expected) {
		    val result: Try[ScoredParticles[T]] = instance1.modelRunner.run(job1)
		    result
      }
	 	}
		"Return no scored parameters if prior density of param is 0" in new Setup {
		val prior = mock[Prior[T]]
		//when(prior.density(T)).thenReturn(0)
		  assertResult() {
		    val result: Try[ScoredParticles[T]] = instance1.modelRunner.run(job1)
      }
	 	}
		"Throw exception when max number of retries reached" in new Setup {
      intercept[MaxRetryException]{
        instance2.modelRunner.run(job2)
      }
	 	}
	}
  
}