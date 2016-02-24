package sampler.abc.actor.main

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import sampler.abc.Scored
import sampler.abc.Generation
import scala.collection.immutable.Queue
import sampler.abc.Weighted
import sampler.abc.Model
import sampler.abc.Prior
import sampler.abc.UseModelPrior
import sampler.abc.Population

class EvolvingGenerationTest extends FreeSpec with Matchers with MockitoSugar {
	type T = Int
	
	"EvolvingGeneration should" - {
		"Initialise with model prior and tolerance infinity" in {
		  val gen0 = UseModelPrior[T]()
			val result = EvolvingGeneration.buildFrom(gen0)

			val expected = EvolvingGeneration(
					Double.MaxValue,
					gen0,
					ScoredParticles.empty,
					WeighedParticles.empty,
					Queue.empty[Long]
			)
			assert(result === expected)
		}
		
		"Initialise with model prior and specific tolerance" in {
		  val gen0 = UseModelPrior[T](9.9999999)
			val result = EvolvingGeneration.buildFrom(gen0)

			val expected = EvolvingGeneration(
					9.9999999,
					gen0,
					ScoredParticles.empty,
					WeighedParticles.empty,
					Queue.empty[Long]
			)
			assert(result === expected)
		}
		
		"Initialise with a previous population and use same tolerance" in {
		  val genN = Population[T](null, 0, 9.999, 0.0)
		  val result = EvolvingGeneration.buildFrom(genN)
		  
		  val expected = EvolvingGeneration(
					genN.tolerance,
					genN,
					ScoredParticles.empty,
					WeighedParticles.empty,
					Queue.empty[Long]
			)
			assert(result === expected)
		}
		
		"Emptying the weighing buffer" in {
	    val scored1 = Tagged(Scored(1, Seq(0.5)), 111111)
	    val scored2 = Tagged(Scored(2, Seq(0.5)), 111112)
	    val weighed1 = Tagged(WeighedParticles.empty)
	  
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