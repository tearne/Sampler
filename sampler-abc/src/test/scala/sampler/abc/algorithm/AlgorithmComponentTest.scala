package sampler.abc.algorithm

import org.mockito.Matchers.anyObject
import org.mockito.Mockito.when
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import akka.event.LoggingAdapter
import sampler.abc.Scored
import sampler.abc.Weighted
import sampler.abc.actor.LoggingAdapterComponent
import sampler.abc.actor.ScoredParticles
import sampler.abc.actor.Tagged
import sampler.abc.actor.WeighedParticles
import sampler.abc.actor.root.Getters
import sampler.abc.actor.root.GettersComponent
import sampler.abc.algorithm.component.ToleranceCalculatorComponent
import sampler.abc.config.ABCConfig
import sampler.abc.config.ClusterParameters
import sampler.abc.config.JobParameters
import sampler.math.Statistics
import sampler.math.StatisticsComponent
import scala.collection.immutable.Queue
import sampler.abc.algorithm.component.ParticleMixerComponent

class AlgorithmComponentTest extends FreeSpec with Matchers with MockitoSugar {

  "Algorithm component should" - {
  
    val instanceComponent = new AlgorithmComponentImpl 
    		with ToleranceCalculatorComponent
    		with StatisticsComponent
    		with LoggingAdapterComponent
    		with ParticleMixerComponent
    		with GettersComponent {
      val statistics = mock[Statistics]
      val getters = new Getters{}
      val toleranceCalculator = mock[ToleranceCalculator]
      val particleMixer = mock[ParticleMixer]
      val logg = mock[LoggingAdapter]
      val algorithm = new AlgorithmImpl{}
    }
    
    val instance = instanceComponent.algorithm
    
    val (id1, id2, id3, id4) = (111111, 111112, 111113, 111114)
    
    val scored1 = Tagged(Scored(1, Seq(0,5)), id1)
    val scored2 = Tagged(Scored(2, Seq(0.5)), id2)
    
    val weighed1 = Tagged(Weighted(Scored(3, Seq(0.25)), 0.25), id3)
    val weighed2 = Tagged(Weighted(Scored(4, Seq(0.25)), 0.25), id4)
    
    "Add incoming weighted particles to a generation" in {
      val initialSeq = WeighedParticles(Seq(weighed1))
      val addedSeq = WeighedParticles(Seq(weighed2))
      
      val gen1 = Generation[Int](
          null,
          ScoredParticles(Seq()),
          initialSeq,
          Queue(),
          0.1,
          1,
          null
      )
      
      val nextGen = instance.addWeighted(addedSeq, gen1)
      val weighedSeq = nextGen.weighted
      
      assert(weighedSeq.seq.length === 2)
      assert(weighedSeq.seq.contains(weighed1))
      assert(weighedSeq.seq.contains(weighed2))
    }
    
    "Filters and queues for weighing" in {
      val scoredSeq = ScoredParticles(Seq(scored1))
      
      val gen1 = Generation[Int](
          null,
          ScoredParticles(Seq()),
          WeighedParticles(Seq()),
          Queue(),
          0.1,
          1,
          null
      )
      
      val nextGen = instance.filterAndQueueForWeighing(scoredSeq, gen1)
      
      val observedIds = nextGen.idsObserved
      val dueWeighing = nextGen.dueWeighing
      
      assert(observedIds.size === 1)
      assert(observedIds.contains(id1))
      
      assert(dueWeighing.size === 1)
      assert(dueWeighing.seq.contains(scored1))
    }
    
    "Similar test to above but with some IDs already present and attempted adding of duplicate" in {
      val initialObs: Queue[Long] = Queue(id1)
      val initialDues = ScoredParticles(Seq(scored1))
      
      val gen1 = Generation[Int](
          null,
          initialDues,
          WeighedParticles(Seq()),
          initialObs,
          0.1,
          1,
          null
      )
      
      val scoredSeq = ScoredParticles(Seq(scored1, scored2))
      
      val nextGen = instance.filterAndQueueForWeighing(scoredSeq, gen1)
      
      val observedIds = nextGen.idsObserved
      val dueWeighing = nextGen.dueWeighing
      
      assert(observedIds.size === 2)
      assert(observedIds.contains(id1))
      assert(observedIds.contains(id2))
      
      assert(dueWeighing.size === 2)
      assert(dueWeighing.seq.contains(scored1))
      assert(dueWeighing.seq.contains(scored2))
    }
    
    "Flushes generation" - {

      "Flushes all elements " in {
        when(instanceComponent.toleranceCalculator.apply(anyObject(), org.mockito.Matchers.eq(0.1))).thenReturn(0.01)
        
        val gen1 = Generation[Int](
          null,
          ScoredParticles(Seq(scored1)),
          WeighedParticles(Seq(weighed1)),
          Queue(id3),
          0.1,
          1,
          null
        )
      
        val nextGen = instance.flushGeneration(gen1, 1, 500)
      
        assert(nextGen.weighted.seq.isEmpty)
        assert(nextGen.currentTolerance === 0.01)
        assert(nextGen.currentIteration === 2)
        assert(nextGen.prevWeightsTable === Map(3 -> 0.25))
        assert(nextGen.dueWeighing.seq.isEmpty)
      }
    
      val numParticles = 2
      val memoryGenerations = 2
      
      "causes assertion error if particles haven't exceeded the memory generations limit" in {
        val shortQueue: Queue[Long] = Queue(id1)
        
        val gen1 = Generation[Int](
            null,
            ScoredParticles(Seq()),
            WeighedParticles(Seq(weighed1)),
            shortQueue,
            0.1,
            1,
            null
        )
        
        intercept[AssertionError]{
          instance.flushGeneration(gen1, numParticles, memoryGenerations)
        }
      }
      
      "reduced to n-1 generations memory if memory limit is exceeded" in {
        val longQueue: Queue[Long] = Queue(id1, id2, id3, id4, 111115)
        
        val gen1 = Generation[Int](
            null,
            ScoredParticles(Seq()),
            WeighedParticles(Seq(weighed1, weighed1, weighed1, weighed1)),
            longQueue,
            0.1,
            1,
            null
        )
        
        val nextGen = instance.flushGeneration(gen1, numParticles, memoryGenerations)
        
        val expectedQueue: Queue[Long] = Queue(id4, 111115)
        
        assert(nextGen.idsObserved === expectedQueue)
      }
      
      "reduced to n-1 generations memory if memory limit is equalled" in {
        val equalQueue: Queue[Long] = Queue(id1, id2, id3, id4)
        
        val gen1 = Generation[Int](
            null,
            ScoredParticles(Seq()),
            WeighedParticles(Seq(weighed1, weighed1, weighed1, weighed1)),
            equalQueue,
            0.1,
            1,
            null
        )
        
        val nextGen = instance.flushGeneration(gen1, numParticles, memoryGenerations)
        
        val expectedQueue: Queue[Long] = Queue(id3, id4)
        
        assert(nextGen.idsObserved === expectedQueue)
      }
    }
    
    "Determine if generation has gathered enough particles" in {
      val config1 = ABCConfig(JobParameters(2,0,0), null, null)
      val config2 = ABCConfig(JobParameters(5,0,0), null, null)
      val config3 = ABCConfig(JobParameters(6,0,0), null, null)
      val config4 = ABCConfig(JobParameters(1000,0,0), null, null)
      
      val gen1 = Generation[Int](
          null,
          null,
          WeighedParticles(Seq(
            weighed1,
            weighed2,
            Tagged(Weighted(Scored(5, Seq(0.5)), 0.5), 111115),
            Tagged(Weighted(Scored(6, Seq(0.5)), 0.5), 111116),
            Tagged(Weighted(Scored(7, Seq(0.5)), 0.5), 111117)
          )),
          Queue(111113, 111114, 111115, 111116, 111117),
          0.1,
          1,
          null
      )
      
      assert(instance.isEnoughParticles(gen1, config1))
      assert(instance.isEnoughParticles(gen1, config2))
      assert(!instance.isEnoughParticles(gen1, config3))
      assert(!instance.isEnoughParticles(gen1, config4))
    }
    
    "Empties weighing buffer" in {
      val gen1 = Generation[Int](
          null,
          ScoredParticles(Seq(scored1)),
          WeighedParticles(Seq()),
          Queue(),
          0.1,
          1,
          null
      )
      
      val nextGen = instance.emptyWeighingBuffer(gen1)
      
      assert(nextGen.dueWeighing.seq.isEmpty)
    }
    
    "Delegates building a mix payload to separate component" - {
      val mixinResponse = Some(ScoredParticles(Seq(scored1, scored2)))
      
      val gen1 = mock[Generation[Int]]
      val config = mock[ABCConfig]
      
      when(instanceComponent.particleMixer.apply(gen1, config)).thenReturn(mixinResponse)
      
      assert(instance.buildMixPayload(gen1, config) === mixinResponse)
    }
    
    "Generates a report" in {
      val gen1 = Generation[Int](
          null,
          ScoredParticles(Seq()),
          WeighedParticles(Seq()),
          Queue(),
          0.001,
          500,
          Map(1 -> 0.5, 2 -> 0.5)
      )
      
      val config = ABCConfig(JobParameters(1000,0,0), null, null)
      
      val report = instance.buildReport(gen1, config)
      
      val posterior = report.posterior
      
      assert(report.generationId === 500)
      assert(report.tolerance === 0.001)
      assert(posterior.length === 1000)
      posterior.count(_ == 1) should be(500 +- 50) 
      posterior.count(_ == 2) should be(500 +- 50) 
    }
  }
}