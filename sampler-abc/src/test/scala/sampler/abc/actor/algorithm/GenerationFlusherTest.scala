package sampler.abc.actor.algorithm

import org.scalatest.FreeSpec
import org.scalatest.mock.MockitoSugar
import org.scalatest.Matchers
import sampler.abc.core.ToleranceCalculator
import sampler.abc.core.WeightsHelper
import sampler.abc.actor.WeighedParticles
import org.mockito.Mockito._
import sampler.abc.Weighted
import sampler.abc.core.Generation
import sampler.abc.Model
import scala.collection.immutable.Queue

class GenerationFlusherTest extends FreeSpec with Matchers with MockitoSugar {
	type T = Int
	val model = mock[Model[T]]
	val tenParticles = 10
	
	trait Setup {
		val toleranceCalculator = mock[ToleranceCalculator]
		val observedIdsTrimmer = mock[ObservedIdsTrimmer]
		val weightsConsolodator = mock[WeightsHelper]
		val getters = mock[Getters]
		def getInstance(numParticles: Int) = 
			new GenerationFlusher(
				toleranceCalculator,
				observedIdsTrimmer,
				weightsConsolodator,
				getters,
				numParticles
			)

		val inProgress = EvolvingGeneration(
			0.1,
			Generation(model, 10, null, 0.5),
			null,
			mock[WeighedParticles[T]],
			mock[Queue[Long]]
		)
		
		val seqWeighed = (1 to 10).map(mock[Seq[Weighted[T]]])
		when(getters.weighedParticlesWithoutIdTags(inProgress.weighed))
			.thenReturn(seqWeighed)

		val weightsTable = mock[Map[T, Double]]
		when(weightsConsolodator.consolidateToWeightsTable(seqWeighed))
			.thenReturn(weightsTable)
			
		val trimmedParticleIds = mock[Queue[Long]]
		when(observedIdsTrimmer.apply(inProgress.idsObserved))
			.thenReturn(trimmedParticleIds)
	}
	
	"should" - {
		"build inner completed generation" in new Setup {
			
			val result = getInstance(tenParticles).apply(inProgress)
			val completedGen = result.previousGen
			
			assert(completedGen.model === model)
			assert(completedGen.iteration === 11)
			assert(completedGen.particleWeights === weightsTable)
			assert(completedGen.tolerance === 0.5)
		}
		
		"build new evolving generation" in new Setup {
			val result = getInstance(tenParticles).apply(inProgress)
			
			assert(result.currentTolerance === 0.1)
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