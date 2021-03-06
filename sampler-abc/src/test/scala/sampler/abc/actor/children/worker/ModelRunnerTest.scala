package sampler.abc.actor.children.worker

import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.when
import org.scalatest.FreeSpec
import org.scalatest.mockito.MockitoSugar
import sampler.abc._
import sampler.abc.actor.children.GenerateParticlesFrom
import sampler.abc.actor.message.ScoredParticles
import sampler.maths.Random

import scala.util.{Success, Try}

class ModelRunnerTest extends FreeSpec with MockitoSugar {
  type T = Int //Pretend model parameters
  val willTerminateAtTargetGen = true //TODO false
  val maxParticleRetries = 3
  val particleChunkSize = 1
  
  class TestableModelRunner(
		val config: ABCConfig)
			extends ModelRunnerComponent[T]
      with AborterComponent {
		val random = Random
		val model: Model[T] = mock[Model[T]]
		val modelRunner = new ModelRunner {}
    val aborter = new Aborter {}
	}

  trait Setup {
    val prevPopulation: Generation[T] = Population(
      Seq(
        Weighted(Scored(1, null), 0.2),
        Weighted(Scored(2, null), 0.8)), 
      0, 
      0, 
      0.0
    )
    
    val config = new ABCConfig(null){
      override lazy val numParticles = 100
      override lazy val numGenerations = 3
      override lazy val terminateAtTargetGen = true
      override lazy val mixRateMS = 0l
    }
      
    val instance1 = new TestableModelRunner(config)
    val job1 = GenerateParticlesFrom[T](prevPopulation, config)
  }
     
  "ModelRunner should / " - {
		"Return scored parameters when given proper proposal distribution" in new Setup {
		  fail("TODO")
		  val expected = Success(Scored(1, Seq(0.4, 0.49, 0.5, 0.51))) // 2/4 under threshold
		  assertResult(expected) {
		    val result: Try[ScoredParticles[T]] = instance1.modelRunner.run(job1)
		    result
      }
	 	}
		"Return no scored parameters if prior density of param is 0" in new Setup {
  		val zeroPrior = mock[Prior[T]]
  		when(zeroPrior.density(any[T])).thenReturn(0, 0, 0.5)  //Two cases of zero prior before success
  		when(instance1.model.prior).thenReturn(zeroPrior)
  	
  		fail("TODO")
  	
		  assertResult(0) {
		    instance1.modelRunner
		      .run(job1)
		      .get
		      .size
      }
	 	}
		"Throw exception when max number of retries reached" in new Setup {
      val config2 = new ABCConfig(null){
    	  override lazy val maxParticleRetries = 0
    		override lazy val numReplicates = 5
    		override lazy val particleChunkSize = 10
      }
        
      val instance2 = new TestableModelRunner(config2)
      val job2 = GenerateParticlesFrom[T](prevPopulation, config2)
		  
      val result = instance2.modelRunner.run(job2)
      assert(result.isFailure)
      intercept[MaxRetryException](result.get)
	 	}
	}
  
}