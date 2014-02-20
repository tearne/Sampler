package sampler.cluster.abc.algorithm

import org.scalatest.Matchers
import org.junit.Test
import sampler.math.Statistics
import sampler.cluster.abc.actor.root.ABCActor
import sampler.cluster.abc.algorithm.component.ToleranceComponent
import sampler.cluster.abc.actor.root.Getters
import sampler.cluster.abc.actor.root.GettersComponent
import akka.actor.ActorLogging
import org.scalatest.FreeSpec
import sampler.math.StatisticsComponent
import org.mockito.Mockito._
import sampler.math.StatisticsComponentImpl
import sampler.cluster.abc.config.ABCConfig
import org.scalatest.mock.MockitoSugar
import sampler.cluster.abc.actor.TaggedWeighedSeq
import sampler.cluster.abc.Scored
import sampler.cluster.abc.Weighted
import sampler.cluster.abc.actor.Tagged
import scala.collection.immutable.SortedSet
import sampler.cluster.abc.actor.TaggedScoredSeq
import sampler.cluster.abc.config.JobParameters
import sampler.cluster.abc.actor.LoggingAdapterComponent
import akka.event.LoggingAdapter
import sampler.cluster.abc.config.ABCParametersTest
import sampler.cluster.abc.config.ClusterParameters

class AlgorithmComponentTest extends FreeSpec with Matchers with MockitoSugar {

  "Algorithm component should" - {
  
    val instanceComponent = new AlgorithmComponentImpl 
    		with ToleranceComponent
    		with StatisticsComponent
    		with LoggingAdapterComponent
    		with GettersComponent {
      val statistics = mock[Statistics]
      val getters = new Getters{}
      val toleranceCalculator = new ToleranceCalculator{}
      val logg = mock[LoggingAdapter]
      val algorithm = new AlgorithmImpl{}
      
       def receive: akka.actor.Actor.Receive = null
    }
    
    val instance = instanceComponent.algorithm
    
    "Add incoming weighted particles to a generation" in {
      val initialSeq = Seq(
    	Tagged(Weighted(Scored(1, Seq(0.5)), 0.5), 111111)
      )
      
      val addedSeq = TaggedWeighedSeq(Seq(
        Tagged(Weighted(Scored(2, Seq(0.5)), 0.5), 111112)
      ))
      
      val incoming: TaggedWeighedSeq[Int] = addedSeq
      val gen1 = Generation[Int](
          null,
          Seq(),
          initialSeq,
          SortedSet(),
          0.1,
          1,
          null
      )
      
      val nextGen = instance.addWeighted(incoming, gen1)
      val weighedSeq = nextGen.weighted
      
      assert(weighedSeq.length === 2)
      assert(weighedSeq(0) === initialSeq.head)
      assert(weighedSeq(1) === addedSeq.seq.head)
    }
    
    "Filters and queues for weighing" in {
      val scoredSeq = TaggedScoredSeq(Seq(
        Tagged(Scored(1, Seq(0,5)), 111111)
      ))
      val gen1 = Generation[Int](
          null,
          Seq(),
          Seq(),
          SortedSet(),
          0.1,
          1,
          null
      )
      
      val nextGen = instance.filterAndQueueForWeighing(scoredSeq, gen1)
      
      val observedIds = nextGen.idsObserved
      val dueWeighing = nextGen.dueWeighing
      
      assert(observedIds.size === 1)
      assert(observedIds.contains(111111))
      
      assert(dueWeighing.length === 1)
      assert(dueWeighing.contains(scoredSeq.seq.head))
    }
    
    "Similar test to above but with some IDs already present and attempted adding of duplicate" in {
      val (id1, id2) = (111111, 111112)
      
      val initialObs: SortedSet[Long] = SortedSet(id1)
      
      val taggedAndScored1 = Tagged(Scored(1, Seq(0.5)), id1)
      val taggedAndScored2 = Tagged(Scored(2, Seq(0.5)), id2)
      
      val initialDues = Seq(
          taggedAndScored1
      )
      
      val gen1 = Generation[Int](
          null,
          initialDues,
          Seq(),
          initialObs,
          0.1,
          1,
          null
      )
      
      val scoredSeq = TaggedScoredSeq(Seq(
        taggedAndScored1,
        taggedAndScored2
      ))
      
      val nextGen = instance.filterAndQueueForWeighing(scoredSeq, gen1)
      
      val observedIds = nextGen.idsObserved
      val dueWeighing = nextGen.dueWeighing
      
      assert(observedIds.size === 2)
      assert(observedIds.contains(id1))
      assert(observedIds.contains(id2))
      
      assert(dueWeighing.length === 2)
      assert(dueWeighing.contains(taggedAndScored1))
      assert(dueWeighing.contains(taggedAndScored2))
    }
    
    "Flushes generation" in {
      val mockedComponent = new AlgorithmComponentImpl 
    		with ToleranceComponent
    		with StatisticsComponent
    		with LoggingAdapterComponent
    		with GettersComponent {
        val statistics = mock[Statistics]
        val getters = new Getters{}
        val toleranceCalculator = mock[ToleranceCalculator]
        val algorithm = new AlgorithmImpl{}
        val logg = mock[LoggingAdapter]
        
        def receive: akka.actor.Actor.Receive = null
        
        import org.mockito.Matchers._
        
        when(toleranceCalculator.apply(anyObject(), org.mockito.Matchers.eq(0.1))).thenReturn(0.01)
      }
      
      val mockedInstance = mockedComponent.algorithm
      
      val gen1 = Generation[Int](
          null,
          Seq(Tagged(Scored(2, Seq(0,5)), 111112)),
          Seq(Tagged(Weighted(Scored(1, Seq(0.5)), 0.5), 111111)),
          SortedSet(111111),
          0.1,
          1,
          null
      )
      
      val nextGen = mockedInstance.flushGeneration(gen1, 1)
      
      assert(nextGen.weighted.isEmpty)
      assert(nextGen.currentTolerance === 0.01)
      assert(nextGen.currentIteration === 2)
      assert(nextGen.prevWeightsTable === Map(1 -> 0.5))
      assert(nextGen.dueWeighing.isEmpty)
//      assert(nextGen.idsObserved.isEmpty)
    }
    
    "Determine if generation has gathered enough particles" in {
      val config1 = ABCConfig(JobParameters(2,0,0), null, null)
      val config2 = ABCConfig(JobParameters(5,0,0), null, null)
      val config3 = ABCConfig(JobParameters(6,0,0), null, null)
      val config4 = ABCConfig(JobParameters(1000,0,0), null, null)
      
      val gen1 = Generation[Int](
          null,
          null,
          Seq(
            Tagged(Weighted(Scored(1, Seq(0.5)), 0.5), 111111),
            Tagged(Weighted(Scored(2, Seq(0.5)), 0.5), 111112),
            Tagged(Weighted(Scored(3, Seq(0.5)), 0.5), 111113),
            Tagged(Weighted(Scored(4, Seq(0.5)), 0.5), 111114),
            Tagged(Weighted(Scored(5, Seq(0.5)), 0.5), 111115)
          ),
          SortedSet(111111, 111112, 111113, 111114, 111115),
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
          Seq(Tagged(Scored(2, Seq(0,5)), 111112)),
          Seq(),
          SortedSet(),
          0.1,
          1,
          null
      )
      
      val nextGen = instance.emptyWeighingBuffer(gen1)
      
      assert(nextGen.dueWeighing.isEmpty)
    }
    
    "Gives previous weights table" in {
      val gen1 = Generation[Int](
          null,
          Seq(),
          Seq(),
          SortedSet(),
          0.1,
          1,
          Map(1 -> 0.25, 2 -> 0.5, 3 -> 0.25)
      )
      
      val previousWeights = instance.weightsTable(gen1)
      
      assert(previousWeights.keySet.size === 3)
//      assert(previousWeights.get(1) === 0.25)
//      assert(previousWeights.getOrElse(1, 0) === 0.25)
//      assert(previousWeights.getOrElse(2, 0) === 0.5)
//      assert(previousWeights.getOrElse(3, 0) === 0.25)
    }
    
    "Builds a mix payload" - {
      "None when no weighteds present" in {
        val gen1 = Generation[Int](
          null,
          Seq(),
          Seq(),
          SortedSet(),
          0.1,
          1,
          null
        )
      
        val config = ABCConfig(
            null,
            null,
            ClusterParameters(false,0,0,2,0,0)
        )
        
        assert(instance.buildMixPayload(gen1, config) === None)
      }
      
      "When some weighteds are present but not bigger than mixing size" in {
        Seq(Tagged(Weighted(Scored(1, Seq(0.5)), 0.5), 111111))
        val gen1 = Generation[Int](
          null,
          Seq(),
          Seq(Tagged(Weighted(Scored(1, Seq(0.5)), 0.5), 111111)),
          SortedSet(),
          0.1,
          1,
          null
        )
        
        val config = ABCConfig(
            null,
            null,
            ClusterParameters(false,0,0,2,0,0)
        )
        
        val taggedScored = instance.buildMixPayload(gen1, config).get
        
        val expected = Tagged(Scored(1, Seq(0.5)), 111111)
        
        assert(taggedScored.seq.size === 1)
        assert(taggedScored.seq.contains(expected))
      }
      
      "When mixing size is exceeded" in {
        val tagged1 = Tagged(Weighted(Scored(1, Seq(0.25)), 0.25), 111111)
        val tagged2 = Tagged(Weighted(Scored(2, Seq(0.25)), 0.25), 111112)
        val tagged3 = Tagged(Weighted(Scored(3, Seq(0.5)), 0.5), 111113)
        
        Seq(Tagged(Weighted(Scored(1, Seq(0.5)), 0.5), 111111))
        val gen1 = Generation[Int](
          null,
          Seq(),
          Seq(tagged1, tagged2, tagged3),
          SortedSet(),
          0.1,
          1,
          null
        )
        
        val config = ABCConfig(
            null,
            null,
            ClusterParameters(false,0,0,2,0,0)
        )
        
        val taggedScored = instance.buildMixPayload(gen1, config).get
        
//        assert(taggedScored.seq.size === 2)	TODO re-implement test when random dependency injection fixed
        assert(taggedScored.seq.size < 3)
      }
    }
    
    "Generates a report" in {
      val gen1 = Generation[Int](
          null,
          Seq(),
          Seq(),
          SortedSet(),
          0.001,
          500,
          Map(1 -> 0.25, 2 -> 0.5, 3 -> 0.25)
      )
      
      val config = ABCConfig(JobParameters(2,0,0), null, null)
      
      val report = instance.buildReport(gen1, config)
      
      assert(report.generationId === 500)
      assert(report.tolerance === 0.001)
      assert(report.posterior.length === 2)
    }
  }
}