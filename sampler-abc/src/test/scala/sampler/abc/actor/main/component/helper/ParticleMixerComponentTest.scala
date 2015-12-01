package sampler.abc.actor.algorithm

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import sampler.abc.config.ABCConfig
import sampler.abc.config.ClusterParameters
import sampler.abc.config.ClusterParameters
import sampler.abc.Scored
import sampler.abc.Weighted
import scala.collection.immutable.Queue
import org.scalatest.BeforeAndAfter
import sampler.math.Random
import sampler.abc.actor.main.EvolvingGeneration
import sampler.abc.actor.main.component.helper.ParticleMixer
import sampler.abc.actor.main.Tagged
import sampler.abc.actor.main.ScoredParticles
import sampler.abc.actor.main.WeighedParticles

class ParticleMixerTest extends FreeSpec with Matchers with MockitoSugar with BeforeAndAfter{

	implicit var r: Random = mock[Random]
	
	def before{
		r = mock[Random]
	}
	
  "ParticleMixer should" - {
    val instance = new ParticleMixer()
    
    val config = mock[ABCConfig]
    val clusterParameters = mock[ClusterParameters]
    
    val payloadSize = 2
    
    when(clusterParameters.mixPayloadSize).thenReturn(payloadSize)
    when(config.cluster).thenReturn(clusterParameters)
    
    val (id1, id2, id3, id4) = (111111, 111112, 111113, 111114)
    
    val scored1 = Tagged(Scored(1, Seq(0.25)), id1)
    val scored2 = Tagged(Scored(2, Seq(0.25)), id2)
    val scored3 = Tagged(Scored(3, Seq(0.25)), id3)
    val scored4 = Tagged(Scored(4, Seq(0.25)), id4)
    
    val weighed1 = Tagged(Weighted(Scored(1, Seq(0.25)), 0.25), id1)
    val weighed2 = Tagged(Weighted(Scored(2, Seq(0.25)), 0.25), id2)
    val weighed3 = Tagged(Weighted(Scored(3, Seq(0.25)), 0.25), id3)
    val weighed4 = Tagged(Weighted(Scored(4, Seq(0.25)), 0.25), id4)
    
    "return None when no weighed particles present in generation" in {
    	//TODO why not just return an empty seq?
      val eGen = EvolvingGeneration[Int](
          0.1,
          null,
          ScoredParticles(Seq()),
          WeighedParticles(Seq()),
          Queue()
        )
        
      assert(instance.apply(eGen, config)(null) === None)
    }
    
    "return all current weighed particles if fewer available than mixing size" in {
      val eGen = EvolvingGeneration[Int](
          0.1,
          null,
          ScoredParticles(Seq()),
          WeighedParticles(Seq(weighed1, weighed2)),
          Queue()
      )
        
      val result = instance.apply(eGen, config).get
      
      assert(result.seq.size === 2)
      assert(result.seq.contains(scored1))
      assert(result.seq.contains(scored2))
    }
    
    "Randomly select from weighteds when more particles present than mix size" in {
      val gen1 = EvolvingGeneration[Int](
          0.1,
          null,
          ScoredParticles(Seq()),
          WeighedParticles(Seq(weighed1, weighed2, weighed3, weighed4)),
          Queue()
        )
      
      val iterations = 1000
        
      //TODO something clever with mock random or distriubtion builder?
      def buildSamples(acc: Seq[Tagged[Scored[Int]]], count:Int = 0, reps: Int = 1000): Seq[Tagged[Scored[Int]]] = {
        if(count >= reps) acc
        else buildSamples(acc ++ instance.apply(gen1, config).get.seq, count+1)
      }
      
      val accum = buildSamples(Seq.empty[Tagged[Scored[Int]]])
      
      val grouped = accum.groupBy(identity).map{case(a, b) => a -> b.size}
      
      assertResult(payloadSize*iterations)(accum.size)
      
      grouped.getOrElse(scored1, 0) should be(500 +- 50)
      grouped.getOrElse(scored2, 0) should be(500 +- 50)
      grouped.getOrElse(scored3, 0) should be(500 +- 50)
      grouped.getOrElse(scored4, 0) should be(500 +- 50)
    }
  }
}