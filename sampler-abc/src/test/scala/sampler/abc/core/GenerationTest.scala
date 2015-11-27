package sampler.abc.core

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import sampler.abc.Scored
import sampler.abc.actor.Tagged
import sampler.abc.actor.ScoredParticles
import sampler.abc.actor.WeighedParticles
import scala.collection.immutable.Queue
import org.scalatest.mock.MockitoSugar
import sampler.abc.Model
import sampler.abc.config.ABCConfig
import org.mockito.Mockito._
import org.mockito.Matchers._
import sampler.abc.config.JobParameters
import sampler.abc.Prior

class GenerationTest extends FreeSpec with Matchers with MockitoSugar {

  "Generation should" - {
   	"initialise" in {
  	  val model = mock[Model[Int]]
      val prior = mock[Prior[Int]]
      when(prior.sample).thenReturn(1,2,3,4)
      when(model.prior).thenReturn(prior)
      when(prior.density(any[Int])).thenReturn(0.25)
    
      val config = mock[ABCConfig]
      val jobParams = mock[JobParameters]
      when(jobParams.numParticles).thenReturn(4)
      when(config.job).thenReturn(jobParams)
  		
      val gen = Generation.init(model, config)
    
      //gen.model shouldBe model
      gen.iteration shouldBe 0
      gen.particleWeights shouldBe Map(
      	1 -> 0.25,
      	2 -> 0.25,
      	3 -> 0.25,
      	4 -> 0.25
      )
      gen.tolerance shouldBe Double.MaxValue
    }
  }
}