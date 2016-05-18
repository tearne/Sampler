//package sampler.abc.actor.main.component
//
//import org.scalatest.FreeSpec
//import org.scalatest.Matchers
//import org.scalatest.mock.MockitoSugar
//import sampler.math.Random
//import sampler.abc.Scored
//import sampler.abc.Weighted
//import scala.collection.immutable.Queue
//import org.mockito.Mockito._
//import org.mockito.Matchers._
//import sampler.abc.actor.main.EvolvingGeneration
//import sampler.abc.actor.sub.flushing.GenerationFlusher
//import sampler.abc.actor.main.component.helper.ParticleMixer
//import sampler.abc.actor.main.component.helper.Getters
//import sampler.abc.actor.main.Tagged
//import sampler.abc.actor.main.WeighedParticles
//import sampler.abc.actor.main.ScoredParticles
//import sampler.abc.ABCConfig
//
//class HelperTest extends FreeSpec with Matchers with MockitoSugar {
//
//	trait Setup {
//		val particleMixer = mock[ParticleMixer]
//		val instance = new Helper(
//				particleMixer,
//				mock[Getters],
//				mock[Random]
//		)
//	}
//	
//  "Helper should" - {
//    val (id1, id2, id3, id4) = (111111, 111112, 111113, 111114)
//    
//    val scored1 = Tagged(Scored(1, Seq(0,5)), id1)
//    val scored2 = Tagged(Scored(2, Seq(0.5)), id2)
//    
//    val weighed1 = Tagged(Weighted(Scored(3, Seq(0.25)), 0.25), id3)
//    val weighed2 = Tagged(Weighted(Scored(4, Seq(0.25)), 0.25), id4)
//    
//    val numRejected1 = 5
//    val numRejected2 = 2
//    
//    "Add incoming weighted particles to a generation" in new Setup {
//      val initialSeq = WeighedParticles(Seq(weighed1), numRejected1)
//      val newWeighedSeq = WeighedParticles(Seq(weighed2), numRejected2)
//      
//      val gen1 = EvolvingGeneration[Int](
//          0.1,
//          null,
//          ScoredParticles(Seq()),
//          initialSeq,
//          Queue.empty[Long]
//      )
//      
//      val result = instance.addWeightedParticles(newWeighedSeq, gen1)
//      val weighedSeq = result.weighed
//      
//      // Note, we are not looking for particles consolidation,  
//      // that comes during flushing.
//      assert(weighedSeq === initialSeq.add(newWeighedSeq))
//    }
//    
//    "Filter and queue scored particles for weighing" in new Setup {
//      val scoredSeq = ScoredParticles(Seq(scored1))
//      
//      val currentTolerance = 0.1
//      val gen1 = EvolvingGeneration[Int](
//          currentTolerance,
//      		null,
//          ScoredParticles.empty,
//          WeighedParticles.empty,
//          Queue()
//      )
//      
//      val nextGen = instance.filterAndQueueUnweighedParticles(scoredSeq, gen1)
//      
//      val observedIds = nextGen.idsObserved
//      val dueWeighing = nextGen.dueWeighing
//      
//      assert(observedIds.size === 1)
//      assert(observedIds.contains(id1))
//      
//      assert(dueWeighing.size === 1)
//      assert(dueWeighing.seq.contains(scored1))
//    }
//    
//    "Filter and queues scored particles with some IDs already presente" in new Setup {
//      val initialObs: Queue[Long] = Queue(id1)
//      val initialDues = ScoredParticles(Seq(scored1))
//      
//      val gen1 = EvolvingGeneration[Int](
//          0.1,
//      		null,
//          initialDues,
//          WeighedParticles.empty,
//          initialObs
//      )
//      
//      val scoredSeq = ScoredParticles(Seq(scored1, scored2))
//      
//      val nextGen = instance.filterAndQueueUnweighedParticles(scoredSeq, gen1)
//      
//      val observedIds = nextGen.idsObserved
//      val dueWeighing = nextGen.dueWeighing
//      
//      assert(observedIds.size === 2)
//      assert(observedIds.contains(id1))
//      assert(observedIds.contains(id2))
//      
//      assert(dueWeighing.size === 2)
//      assert(dueWeighing.seq.contains(scored1))
//      assert(dueWeighing.seq.contains(scored2))
//    }
//    
//    "Determine if generation has gathered enough particles" in new Setup {
//      val config1 = new ABCConfig(null){ override lazy val numParticles = 2 }
//      val config2 = new ABCConfig(null){ override lazy val numParticles = 5 }
//      val config3 = new ABCConfig(null){ override lazy val numParticles = 6 }
//      val config4 = new ABCConfig(null){ override lazy val numParticles = 1000 }
//      
//      val gen1 = EvolvingGeneration[Int](
//          0.0,
//          null,
//          null,
//          WeighedParticles(
//              Seq(
//                weighed1,
//                weighed2,
//                Tagged(Weighted(Scored(5, Seq(0.5)), 0.5), 111115),
//                Tagged(Weighted(Scored(6, Seq(0.5)), 0.5), 111116),
//                Tagged(Weighted(Scored(7, Seq(0.5)), 0.5), 111117)
//              ),
//              numRejected1),
//          null
//      )
//      
//      assert(instance.isEnoughParticles(gen1, config1))
//      assert(instance.isEnoughParticles(gen1, config2))
//      assert(!instance.isEnoughParticles(gen1, config3))
//      assert(!instance.isEnoughParticles(gen1, config4))
//    }
//    
//    "Emptying weighing buffer" in new Setup {
//      val dueWeighing = ScoredParticles(Seq(scored1))
//    	val gen1 = EvolvingGeneration[Int](
//          0.0,
//      		null,
//          dueWeighing,
//          null,
//          null
//      )
//      
//      val nextGen = instance.emptyWeighingBuffer(gen1)
//      
//      assert(nextGen.dueWeighing.seq.isEmpty)
//    }
//    
//    "Delegates building a mix payload" in new Setup {
//      val mixinResponse = Some(ScoredParticles(Seq(scored1, scored2)))
//      
//      val gen1 = mock[EvolvingGeneration[Int]]
//      val config = mock[ABCConfig]
//      
//      when(particleMixer.apply(
//          org.mockito.Matchers.eq(gen1), 
//          org.mockito.Matchers.eq(config)
//      )(
//          any[Random]
//      )).thenReturn(mixinResponse)
//      
//      assert(instance.buildMixPayload(gen1, config) === mixinResponse)
//    }
//  }
//}