package sampler.abc.actor.root.state.task.egen

import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FreeSpec, Matchers}
import sampler.abc._
import sampler.abc.actor.message.{ScoredParticles, WeighedParticles}

import scala.collection.immutable.Queue

class EvolvingGenerationTest extends FreeSpec with Matchers with MockitoSugar {
	type T = Int
	
	"EvolvingGeneration should" - {
	  "Combine current and a previous generations particles in mix pool" in {
	    val currentParticles = (1 to 3).map(_ => mock[Weighted[Int]]).toSeq
	    val previousParticles = (1 to 3).map(_ => mock[Weighted[Int]]).toSeq
	    
	    val instance = EvolvingGeneration(
	        0,
	        Population(previousParticles, 0, 0, 0),
	        null,
	        WeighedParticles(currentParticles, 0),
	        null
	      )
	    
	    assert(instance.mixingPool === currentParticles ++ previousParticles)
	  }
	  
	  "Only use current particles for mix pool if previous gen was a prior" in {
	    val currentParticles = (1 to 3).map(_ => mock[Weighted[Int]]).toSeq
	    
	    val instance = EvolvingGeneration(
	        0,
	        UseModelPrior(0),
	        null,
	        WeighedParticles(currentParticles, 0),
	        null
	      )
	    
	    assert(instance.mixingPool === currentParticles)
	  }
		
		"Emptying the weighing buffer" in {
	    val scored1 = Scored(1, Seq(0.5))
	    val scored2 = Scored(2, Seq(0.5))
	    val weighed1 = WeighedParticles.empty
	  
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
			
			eGenNew shouldBe expected
	  }
	}
}