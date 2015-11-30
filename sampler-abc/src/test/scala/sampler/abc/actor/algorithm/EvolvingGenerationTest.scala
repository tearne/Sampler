package sampler.abc.actor.algorithm

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import sampler.abc.actor.Tagged
import sampler.abc.Scored
import sampler.abc.core.Generation
import scala.collection.immutable.Queue
import sampler.abc.actor.message.ScoredParticles
import sampler.abc.actor.message.WeighedParticles
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EvolvingGenerationTest extends FreeSpec with Matchers with MockitoSugar {
	type T = Int
	
	"EvolvingGeneration should" - {
		"Emptying its weighing buffer" in {
      val scored1 = Tagged(Scored(1, Seq(0.5)), 111111)
      val scored2 = Tagged(Scored(2, Seq(0.5)), 111112)
      val weighed1 = Tagged(WeighedParticles(Seq(null, null)))
    
      val prevGen = mock[Generation[T]]
      val idsObserved = mock[Queue[Long]]
      val scored = mock[ScoredParticles[T]]
      val weighed = mock[WeighedParticles[T]]
      val eGen = EvolvingGeneration[T](
				0.1,
				prevGen,
				scored,	//items in the weighing buffer
				weighed,
				idsObserved
			)
    
      val eGenNew = eGen.emptyWeighingBuffer
    
      val expected = EvolvingGeneration[T](
				0.1,
				prevGen,
				ScoredParticles.empty,
				weighed,
				idsObserved
			)
			
			fail("check works")
			eGenNew shouldBe expected
      
      //{
      //	import eGenNew._
      //	dueWeighing.size shouldBe 2
      //	previousGen should be theSameInstanceAs prevGen
      //}
    }
	}
}