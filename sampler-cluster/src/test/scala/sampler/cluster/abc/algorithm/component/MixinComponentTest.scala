package sampler.cluster.abc.algorithm.component

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import sampler.math.StatisticsComponent
import sampler.math.Statistics
import sampler.cluster.abc.algorithm.Generation
import sampler.cluster.abc.actor.ScoredParticles
import sampler.cluster.abc.actor.WeighedParticles
import scala.collection.immutable.Queue
import sampler.cluster.abc.config.ABCConfig
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.Weighted
import sampler.cluster.abc.Scored
import sampler.cluster.abc.config.ClusterParameters

class MixinComponentTest extends FreeSpec with Matchers with MockitoSugar {

  "Mixin Component should" - {
    
    val instance = new MixinComponent with StatisticsComponent {
      val statistics = Statistics
      val mixin = new Mixin{}
    }
    
    val config = mock[ABCConfig]
    val clusterParameters = mock[ClusterParameters]
    
    when(clusterParameters.mixPayloadSize).thenReturn(2)
    when(config.cluster).thenReturn(clusterParameters)
    
    val mixin = instance.mixin
    
    val (id1, id2, id3, id4) = (111111, 111112, 111113, 111114)
    
    val scored1 = Tagged(Scored(1, Seq(0.25)), id1)
    val scored2 = Tagged(Scored(2, Seq(0.25)), id2)
    val scored3 = Tagged(Scored(3, Seq(0.25)), id3)
    val scored4 = Tagged(Scored(4, Seq(0.25)), id4)
    
    val weighed1 = Tagged(Weighted(Scored(1, Seq(0.25)), 0.25), id1)
    val weighed2 = Tagged(Weighted(Scored(2, Seq(0.25)), 0.25), id2)
    val weighed3 = Tagged(Weighted(Scored(3, Seq(0.25)), 0.25), id3)
    val weighed4 = Tagged(Weighted(Scored(4, Seq(0.25)), 0.25), id4)
    
    "Returns None when no weighed particles present in generation" in {
      val gen1 = Generation[Int](
          null,
          ScoredParticles(Seq()),
          WeighedParticles(Seq()),
          Queue(),
          0.1,
          1,
          null
        )
        
      assert(mixin.apply(gen1, config) === None)
    }
    
    "Returns the current weighed particles as scored when those present don't exceed mixing size" in {
      val gen1 = Generation[Int](
          null,
          ScoredParticles(Seq()),
          WeighedParticles(Seq(weighed1, weighed2)),
          Queue(),
          0.1,
          1,
          null
        )
        
      val result = mixin.apply(gen1, config).get
      
      assert(result.seq.size === 2)
      assert(result.seq.contains(scored1))
      assert(result.seq.contains(scored2))
    }
    
    "Randomly selects from weighteds when more particles present than mixin size" in {
      val gen1 = Generation[Int](
          null,
          ScoredParticles(Seq()),
          WeighedParticles(Seq(weighed1, weighed2, weighed3, weighed4)),
          Queue(),
          0.1,
          1,
          null
        )
      
      def buildSamples(acc: Seq[Tagged[Scored[Int]]], count:Int = 0, reps: Int = 1000): Seq[Tagged[Scored[Int]]] = {
        if(count >= reps) acc
        else{
          buildSamples(acc ++ mixin.apply(gen1, config).get.seq, count+1)
        }
      }
      
      var accum = buildSamples(Seq.empty[Tagged[Scored[Int]]])
      
      val grouped = accum.groupBy(x => x).map{case(a, b) => a -> b.size}
      
      grouped.getOrElse(scored1, 0) should be(500 +- 50)
      grouped.getOrElse(scored2, 0) should be(500 +- 50)
      grouped.getOrElse(scored3, 0) should be(500 +- 50)
      grouped.getOrElse(scored4, 0) should be(500 +- 50)
    }
  }
}