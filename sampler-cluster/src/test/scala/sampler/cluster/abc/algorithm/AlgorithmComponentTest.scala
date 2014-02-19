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
      
      val config = mock[ABCConfig]
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
      
      val nextGen = instance.addWeighted(incoming, gen1, config)
      val weighedSeq = nextGen.weighted
      
      println(weighedSeq)
      
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
//      assert(nextGen.dueWeighing.isEmpty)
//      assert(nextGen.idsObserved.isEmpty)
    }
    
//    "Determine if generation has gathered enough particles" in {
//      val config1 = ABCConfig(JobParameters(2,0,0), null, null)
//      val config2 = ABCConfig(JobParameters(3,0,0), null, null)
//      val config3 = ABCConfig(JobParameters(5,0,0), null, null)
//      val config4 = ABCConfig(JobParameters(1000,0,0), null, null)
//      
//      val gen1 = Generation[Int](
//          null,
//          null,
//          Seq(Tagged(Weighted(Scored(1, Seq(0.5)), 0.5), 111111)),
//          SortedSet(111111),
//          0.1,
//          1,
//          null
//      )
//    }
  }
}