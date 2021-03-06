package sampler.abc.actor.children.flushing

import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FreeSpec, Matchers}
import sampler.abc.actor.message.WeighedParticles
import sampler.abc.actor.root.state.task.egen.EvolvingGeneration
import sampler.abc._

import scala.collection.immutable.Queue

class GenerationFlusherTest extends FreeSpec with Matchers with MockitoSugar {
	type T = Int
	val model = mock[Model[T]]
	val tenParticles = 10
	
	trait Setup {
	  val numParticlesReqd: Int
		val toleranceCalculator = mock[ToleranceCalculator]
		val config = new ABCConfig(null){
		  override lazy val numParticles = numParticlesReqd
	  }
		
		val instance: GenerationFlusher = 
		  new GenerationFlusher(
				toleranceCalculator,
				config
			)

		val weighedParticles = mock[WeighedParticles[T]]
		when(weighedParticles.size).thenReturn(10)
		when(weighedParticles.acceptanceRatio).thenReturn(0.75)
		val seqWeighed = (1 to 10).map(mock[Seq[Weighted[T]]])
		when(weighedParticles.seq).thenReturn(seqWeighed)
		val inProgress = EvolvingGeneration(
			0.1,
			Population(null, 10, 0.5, 0.75),
			null,
			weighedParticles,
			mock[Queue[UUID]]
		)
		
		when(toleranceCalculator.apply(seqWeighed, config, 0.1)).thenReturn(0.001)
	}
	
	"should" - {
		"build inner completed generation" in new Setup {
			val numParticlesReqd = 10
			
			val result: EvolvingGeneration[T] = instance.fromEvolvingGen(inProgress)
			val completedGen = result.previousGen.asInstanceOf[Population[T]]
			
			assert(completedGen.iteration === 11)
			assert(completedGen.weightedParticles === seqWeighed)
			assert(completedGen.tolerance === 0.1)
			assert(completedGen.acceptanceRatio === 0.75)
		}
		
		"be ok with flushing more than the required number of particles" in new Setup {
		  val numParticlesReqd = 5
			
			val result: EvolvingGeneration[T] = instance.fromEvolvingGen(inProgress)
			val completedGen = result.previousGen.asInstanceOf[Population[T]]
			
			assert(completedGen.weightedParticles === seqWeighed)
		}
		
		"build new evolving generation" in new Setup {
		  val numParticlesReqd = 10
			val result = instance.fromEvolvingGen(inProgress)
			
			assert(result.currentTolerance === 0.001)
			assert(result.dueWeighing.size === 0)
			assert(result.weighed.size === 0)
			assert(result.weighed.numLocalParticlesRejected === 0)
			assert(result.idsObserved === inProgress.idsObserved)
		}
		
		"throw exception if insufficient particles" in new Setup {
			val numParticlesReqd = 11
			intercept[AssertionError]{
				val result = instance.fromEvolvingGen(inProgress)
			}
		}
		
		"carry over any unweighed particles for the next generation rather than throw away" in pending
	}
}