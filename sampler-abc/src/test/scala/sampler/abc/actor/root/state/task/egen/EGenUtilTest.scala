package sampler.abc.actor.root.state.task.egen

import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FreeSpec, Matchers}
import sampler.abc._
import sampler.abc.actor.message.{ScoredParticles, WeighedParticles}
import sampler.maths.Random

import scala.collection.immutable.Queue

class EGenUtilTest extends FreeSpec with Matchers with MockitoSugar {

  trait Setup {
    val particleMixer = mock[ParticleMixer]
    val observedIdsTrimmer = mock[ObservedIdsTrimmer]
    val instance = new EGenUtil(
      particleMixer,
      mock[Random],
      observedIdsTrimmer
    )
  }

  "Helper should" - {
    val (id1, id2, id3, id4) = (UUID.generate, UUID.generate, UUID.generate, UUID.generate)

    val scored1 = Scored(1, Seq(0, 5), Some(id1))
    val scored2 = Scored(2, Seq(0.5), Some(id2))

    val weighed1 = Weighted(Scored(3, Seq(0.25), Some(id3)), 0.25)
    val weighed2 = Weighted(Scored(4, Seq(0.25), Some(id4)), 0.25)

    val numRejected1 = 5
    val numRejected2 = 2

    "Add incoming weighted particles to a generation" in new Setup {
      val initialSeq = WeighedParticles(Seq(weighed1), numRejected1)
      val newWeighedSeq = WeighedParticles(Seq(weighed2), numRejected2)

      val gen1 = EvolvingGeneration[Int](
        0.1,
        null,
        ScoredParticles(Seq()),
        initialSeq,
        Queue.empty
      )

      val result = instance.addWeightedParticles(newWeighedSeq, gen1)
      val weighedSeq = result.weighed

      // Note, we are not looking for particle consolidation,
      // that comes during flushing.
      assert(weighedSeq === initialSeq.add(newWeighedSeq))
    }

    "Filter and queue scored particles for weighing" in new Setup {
      val scoredSeq = ScoredParticles(Seq(scored1, scored2))

      val currentTolerance = 0.1
      val gen1 = EvolvingGeneration[Int](
        currentTolerance,
        null,
        ScoredParticles.empty,
        WeighedParticles.empty,
        Queue()
      )

      when(observedIdsTrimmer.apply(Queue(id1, id2))).thenReturn(Queue(id1)) //Simulate some trimming of observed ids
      val nextGen = instance.filterAndQueueUnweighedParticles(scoredSeq, gen1)

      val observedIds = nextGen.idsObserved
      val dueWeighing = nextGen.dueWeighing

      assert(observedIds === Queue(id1))
      assert(dueWeighing === scoredSeq)
    }

    "Filter and queue scored particles with some IDs already present" in new Setup {
      val idsAlreadyObserved: Queue[UUID] = Queue(id1, id3)
      val initialDueWeighing = ScoredParticles(Seq(scored1))

      val gen1 = EvolvingGeneration[Int](
        0.1,
        null,
        initialDueWeighing,
        WeighedParticles.empty,
        idsAlreadyObserved
      )

      val scoredSeq = ScoredParticles(Seq(scored1, scored2)) //Seen 1 already, 2 is new
      when(observedIdsTrimmer.apply(Queue(id1, id3, id2))).thenReturn(Queue(id3, id2)) //id2 trimmed out
      val nextGen = instance.filterAndQueueUnweighedParticles(scoredSeq, gen1)

      val observedIds = nextGen.idsObserved
      val dueWeighing = nextGen.dueWeighing

      assert(observedIds === Queue(id3, id2))

      assert(dueWeighing.size === 2)
      assert(dueWeighing.seq.contains(scored1))
      assert(dueWeighing.seq.contains(scored2))
    }

    "Determine if generation has gathered enough particles" in new Setup {
      val config1 = new ABCConfig(null) {
        override lazy val numParticles = 2
      }
      val config2 = new ABCConfig(null) {
        override lazy val numParticles = 5
      }
      val config3 = new ABCConfig(null) {
        override lazy val numParticles = 6
      }
      val config4 = new ABCConfig(null) {
        override lazy val numParticles = 1000
      }

      val gen1 = EvolvingGeneration[Int](
        0.0,
        null,
        null,
        WeighedParticles(
          Seq(
            weighed1,
            weighed2,
            Weighted(Scored(5, Seq(0.5), Some(UUID.generate)), 0.5),
            Weighted(Scored(6, Seq(0.5), Some(UUID.generate)), 0.5),
            Weighted(Scored(7, Seq(0.5), Some(UUID.generate)), 0.5)
          ),
          numRejected1),
        null
      )

      assert(instance.isEnoughParticles(gen1, config1))
      assert(instance.isEnoughParticles(gen1, config2))
      assert(!instance.isEnoughParticles(gen1, config3))
      assert(!instance.isEnoughParticles(gen1, config4))
    }

    "Emptying weighing buffer" in new Setup {
      val dueWeighing = ScoredParticles(Seq(scored1))
      val gen1 = EvolvingGeneration[Int](
        0.0,
        null,
        dueWeighing,
        null,
        null
      )

      val nextGen = instance.emptyWeighingBuffer(gen1)

      assert(nextGen.dueWeighing.seq.isEmpty)
    }

    "Delegates building a mix payload" in new Setup {
      val mixinResponse = Some(ScoredParticles(Seq(scored1, scored2)))

      val gen1 = mock[EvolvingGeneration[Int]]
      val config = mock[ABCConfig]

      when(particleMixer.apply(
        org.mockito.Matchers.eq(gen1),
        org.mockito.Matchers.eq(config)
      )(
        any[Random]
      )).thenReturn(mixinResponse)

      assert(instance.buildMixPayload(gen1, config) === mixinResponse)
    }
  }
}