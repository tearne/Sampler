package sampler.abc.actor.algorithm

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import sampler.math.Random
import sampler.abc.actor.Tagged
import sampler.abc.Scored
import sampler.abc.Weighted
import sampler.abc.actor.message.WeighedParticles
import sampler.abc.actor.message.ScoredParticles
import scala.collection.immutable.Queue
import org.mockito.Mockito._
import org.mockito.Matchers._
import sampler.abc.config.ABCConfig
import sampler.abc.config.JobParameters

class AlgorithmTest extends FreeSpec with Matchers with MockitoSugar {

	trait Setup {
		val generationFlusher = mock[GenerationFlusher]
		val particleMixer = mock[ParticleMixer]
		val getters = mock[Getters]
		val random = mock[Random]
		val instance = new Algorithm(
				generationFlusher,
				particleMixer,
				getters,
				random
		)
	}
	
  "Algorithm component should" - {
    val (id1, id2, id3, id4) = (111111, 111112, 111113, 111114)
    
    val scored1 = Tagged(Scored(1, Seq(0,5)), id1)
    val scored2 = Tagged(Scored(2, Seq(0.5)), id2)
    
    val weighed1 = Tagged(Weighted(Scored(3, Seq(0.25)), 0.25), id3)
    val weighed2 = Tagged(Weighted(Scored(4, Seq(0.25)), 0.25), id4)
    
    "Add incoming weighted particles to a generation" in new Setup {
      val initialSeq = WeighedParticles(Seq(weighed1))
      val newWeighedSeq = WeighedParticles(Seq(weighed2))
      
      val gen1 = EvolvingGeneration[Int](
          0.1,
          null,
          ScoredParticles(Seq()),
          initialSeq,
          Queue.empty[Long]
      )
      
      val result = instance.addWeightedParticles(newWeighedSeq, gen1)
      val weighedSeq = result.weighed
      
      assert(weighedSeq.seq.length === 2)
      assert(weighedSeq.seq.contains(weighed1))
      assert(weighedSeq.seq.contains(weighed2))
    }
    
    "Filter and queue scored particles for weighing" in new Setup {
      val scoredSeq = ScoredParticles(Seq(scored1))
      
      val gen1 = EvolvingGeneration[Int](
          0.1,
      		null,
          ScoredParticles(Seq()),
          WeighedParticles(Seq()),
          Queue()
      )
      
      val nextGen = instance.filterAndQueueUnweighedParticles(scoredSeq, gen1)
      
      val observedIds = nextGen.idsObserved
      val dueWeighing = nextGen.dueWeighing
      
      assert(observedIds.size === 1)
      assert(observedIds.contains(id1))
      
      assert(dueWeighing.size === 1)
      assert(dueWeighing.seq.contains(scored1))
    }
    
    "Filter and queues scored particles with some IDs already presente" in new Setup {
      val initialObs: Queue[Long] = Queue(id1)
      val initialDues = ScoredParticles(Seq(scored1))
      
      val gen1 = EvolvingGeneration[Int](
          0.1,
      		null,
          initialDues,
          WeighedParticles(Seq()),
          initialObs
      )
      
      val scoredSeq = ScoredParticles(Seq(scored1, scored2))
      
      val nextGen = instance.filterAndQueueUnweighedParticles(scoredSeq, gen1)
      
      val observedIds = nextGen.idsObserved
      val dueWeighing = nextGen.dueWeighing
      
      assert(observedIds.size === 2)
      assert(observedIds.contains(id1))
      assert(observedIds.contains(id2))
      
      assert(dueWeighing.size === 2)
      assert(dueWeighing.seq.contains(scored1))
      assert(dueWeighing.seq.contains(scored2))
    }
    
    "Delegate flushing of completed generations" in new Setup {
    	val eGen = mock[EvolvingGeneration[Int]]
    	val flushedEGen = mock[EvolvingGeneration[Int]]
    	
    	when(generationFlusher.apply(eGen)).thenReturn(flushedEGen)
    	assert(instance.flushGeneration(eGen) === flushedEGen)
    }
    
    "Determine if generation has gathered enough particles" in new Setup {
      val config1 = ABCConfig(JobParameters(2,0,0), null, null)
      val config2 = ABCConfig(JobParameters(5,0,0), null, null)
      val config3 = ABCConfig(JobParameters(6,0,0), null, null)
      val config4 = ABCConfig(JobParameters(1000,0,0), null, null)
      
      val gen1 = EvolvingGeneration[Int](
          0.0,
          null,
          null,
          WeighedParticles(Seq(
            weighed1,
            weighed2,
            Tagged(Weighted(Scored(5, Seq(0.5)), 0.5), 111115),
            Tagged(Weighted(Scored(6, Seq(0.5)), 0.5), 111116),
            Tagged(Weighted(Scored(7, Seq(0.5)), 0.5), 111117)
          )),
          null
      )
      
      assert(instance.isEnoughParticles(gen1, config1))
      assert(instance.isEnoughParticles(gen1, config2))
      assert(!instance.isEnoughParticles(gen1, config3))
      assert(!instance.isEnoughParticles(gen1, config4))
    }
    
    "Emptying weighing buffer" in new Setup {
      val gen1 = EvolvingGeneration[Int](
          0.0,
      		null,
          ScoredParticles(Seq(scored1)),
          null,
          null
      )
      
      val nextGen = instance.emptyWeighingBuffer(gen1)
      
      assert(nextGen.dueWeighing.seq.isEmpty)
    }
    
    "Delegates building a mix payload to separate component" in new Setup {
      val mixinResponse = Some(ScoredParticles(Seq(scored1, scored2)))
      
      val gen1 = mock[EvolvingGeneration[Int]]
      val config = mock[ABCConfig]
      
      when(particleMixer.apply(gen1, config)(any[Random])).thenReturn(mixinResponse)
      
      assert(instance.buildMixPayload(gen1, config) === mixinResponse)
    }
  }
}