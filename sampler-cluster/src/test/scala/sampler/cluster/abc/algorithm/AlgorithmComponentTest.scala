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

class AlgorithmComponentTest extends FreeSpec with Matchers with MockitoSugar {

  "Algorithm component should" - {
  
    val instanceComponent = new AlgorithmComponentImpl 
    		with ToleranceComponent
    		with StatisticsComponent
    		with GettersComponent {
      val statistics = mock[Statistics]
      val getters = new Getters{}
      val toleranceCalculator = new ToleranceCalculator{}
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
        Tagged(Scored(1, Seq(0,5)) , 111111)
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
      
      assert(dueWeighing.length === 2)
      assert(dueWeighing.contains(scoredSeq.seq.head))
    }
    
    "Similar test to above but with some IDs already present and attempted adding of duplicate" in {
      
    }
    
    "Flushes generation" in {
      // set up to return correct next generation
      
      val nextGen = mock[Generation[Int]]
      
      assert(nextGen.weighted.isEmpty)
      assert(nextGen.currentTolerance === 0.0) // mock return
      assert(nextGen.currentIteration === 2)
      assert(nextGen.prevWeightsTable === Map(1 -> 1.0))
    }
  }
}