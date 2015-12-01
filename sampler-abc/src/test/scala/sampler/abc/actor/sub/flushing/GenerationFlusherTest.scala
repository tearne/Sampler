package sampler.abc.actor.sub.flushing

import org.scalatest.FreeSpec
import org.scalatest.mock.MockitoSugar
import org.scalatest.Matchers
import org.mockito.Mockito._
import sampler.abc.Weighted
import sampler.abc.Generation
import sampler.abc.Model
import scala.collection.immutable.Queue
import sampler.abc.Population
import sampler.abc.actor.main.EvolvingGeneration
import sampler.abc.actor.main.component.helper.Getters

class GenerationFlusherTest extends FreeSpec with Matchers with MockitoSugar {
	type T = Int
	val model = mock[Model[T]]
	val tenParticles = 10
	
	trait Setup {
		val toleranceCalculator = mock[ToleranceCalculator]
		val observedIdsTrimmer = mock[ObservedIdsTrimmer]
		val weightsConsolodator = mock[WeightsHelper]
		val getters = mock[Getters]
		def getInstance(numParticlesRequired: Int): GenerationFlusher = 
			new GenerationFlusher(
				toleranceCalculator,
				observedIdsTrimmer,
				weightsConsolodator,
				getters,
				numParticlesRequired
			)

		val inProgress = EvolvingGeneration(
			0.1,
			Population(null, 10, 0.5),
			null,
			mock[WeighedParticles[T]],
			mock[Queue[Long]]
		)
		
		val seqWeighed = (1 to 10).map(mock[Seq[Weighted[T]]])
		when(getters.weighedParticlesWithoutIdTags(inProgress.weighed))
			.thenReturn(seqWeighed)
		when(toleranceCalculator.apply(seqWeighed, 0.1)).thenReturn(0.001)

		val weightsTable = mock[Map[T, Double]]
		when(weightsConsolodator.consolidateToWeightsTable(seqWeighed))
			.thenReturn(weightsTable)
			
		val trimmedParticleIds = mock[Queue[Long]]
		when(observedIdsTrimmer.apply(inProgress.idsObserved))
			.thenReturn(trimmedParticleIds)
	}
	
	"should" - {
		"build inner completed generation" in new Setup {
			val instance = getInstance(tenParticles)
			
			val result: EvolvingGeneration[T] = instance.apply(inProgress)
			val completedGen = result.previousGen.asInstanceOf[Population[T]]
			
			//assert(completedGen.model === model)
			assert(completedGen.iteration === 11)
			assert(completedGen.particleWeights === weightsTable)
			assert(completedGen.tolerance === 0.1)
		}
		
		"build new evolving generation" in new Setup {
			val result = getInstance(tenParticles).apply(inProgress)
			
			assert(result.currentTolerance === 0.001)
			assert(result.dueWeighing.size === 0)
			assert(result.weighed.size === 0)
			assert(result.idsObserved === trimmedParticleIds)
		}
		
		"throw exception if insufficient particles" in new Setup {
			val elevenParticles = 11
			intercept[AssertionError]{
				val result = getInstance(elevenParticles).apply(inProgress)
			}
		}
	}
}