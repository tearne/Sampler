package sampler.cluster.abc.algorithm

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import sampler.cluster.abc.Scored
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.actor.ScoredParticles
import sampler.cluster.abc.actor.WeighedParticles
import scala.collection.immutable.Queue
import org.scalatest.mock.MockitoSugar
import sampler.cluster.abc.Model
import sampler.cluster.abc.config.ABCConfig
import org.mockito.Mockito._
import org.mockito.Matchers._
import sampler.cluster.abc.config.JobParameters
import sampler.cluster.abc.Prior

class GenerationTest extends FreeSpec with Matchers with MockitoSugar {

  "Generation should" - {
  	"Emptying weighing buffer" in {
      val scored1 = Tagged(Scored(1, Seq(0.5)), 111111)
      val scored2 = Tagged(Scored(2, Seq(0.5)), 111112)
    
      val gen1 = Generation(
        null,
        ScoredParticles(Seq(scored1, scored2)),	//items in the buffer
        WeighedParticles(Seq()),
        Queue(),
        0.1,
        1,
        null
      )
    
      val nextGen = gen1.emptyWeighingBuffer
    
      assert(gen1.dueWeighing.size == 2)
      assert(nextGen.dueWeighing.size == 0)
    }
  	
  	"Initialising" in {
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
    
      assertResult(model)(gen.model)
      assertResult(0)(gen.dueWeighing.seq.size)
      assertResult(0)(gen.weighted.seq.size)
      assertResult(0)(gen.idsObserved.size)
      assertResult(Double.MaxValue)(gen.currentTolerance)
      assertResult(0)(gen.currentIteration)
    
      val tolerance = 1e-6
      val weightsMap = gen.prevWeightsTable
    
      assertResult(0.25)(weightsMap.getOrElse(1, 0))
      assertResult(0.25)(weightsMap.getOrElse(2, 0))
      assertResult(0.25)(weightsMap.getOrElse(3, 0))
      assertResult(0.25)(weightsMap.getOrElse(4, 0))
    }
  }
}