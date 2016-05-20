package sampler.abc.actor.main.component.helper

import scala.collection.immutable.Queue

import org.scalatest.BeforeAndAfter
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar

import sampler.abc.ABCConfig
import sampler.abc.Scored
import sampler.abc.Weighted
import sampler.abc.actor.main.EvolvingGeneration
import sampler.abc.actor.main.ScoredParticles
import sampler.abc.actor.main.WeighedParticles
import sampler.math.Random

class ParticleMixerTest extends FreeSpec with Matchers with MockitoSugar with BeforeAndAfter{

  "ParticleMixer should" - {
    val instance = new ParticleMixer()
    
    val config = new ABCConfig(null){
      override lazy val mixPayloadSize = 2
    }
    
    val (id1, id2, id3, id4) = (111111, 111112, 111113, 111114)
    
    val scored1 = Scored(1, Seq(1,1,1,1), id1)
    val scored2 = Scored(2, Seq(2,2,2,2), id2)
    val scored3 = Scored(3, Seq(3,3,3,3), id3)
    val scored4 = Scored(4, Seq(4,4,4,4), id4)
    
    val weighed1 = Weighted(scored1, 0.25)
    val weighed2 = Weighted(scored2, 0.25)
    val weighed3 = Weighted(scored3, 0.25)
    val weighed4 = Weighted(scored4, 0.25)
    
    val irrelevant = 1
    
    "return None when no weighed particles present in generation" in {
    	//TODO why not just return an empty seq?
      val eGen = EvolvingGeneration[Int](
          0.1,
          null,
          ScoredParticles.empty,
          WeighedParticles.empty,
          Queue()
        )
        
      assert(instance.apply(eGen, config)(null) === None)
    }
    
    "return all current weighed particles if fewer available than mixing size" in {
      val eGen = EvolvingGeneration[Int](
          0.1,
          null,
          ScoredParticles(Seq()),
          WeighedParticles(Seq(weighed1, weighed2), irrelevant),
          Queue()
      )
        
      val result = instance.apply(eGen, config)(null).get
      
      assert(result.seq.size === 2)
      assert(result.seq.contains(scored1))
      assert(result.seq.contains(scored2))
    }
    
    "randomly select from weighted particles when more present than required" in {
      val gen1 = EvolvingGeneration[Int](
          0.1,
          null,
          ScoredParticles(Seq()),
          WeighedParticles(Seq(weighed1, weighed2, weighed3, weighed4), irrelevant),
          Queue()
        )
      
        println(gen1)
      val iterations = 10
        
      //TODO something clever with mock random or distriubtion builder?
      def buildSamples(acc: Seq[Scored[Int]], count:Int = 0, reps: Int = iterations): Seq[Scored[Int]] = {
        if(count >= reps) acc
        else buildSamples(acc ++ instance.apply(gen1, config)(Random).get.seq, count+1)
      }
      
      val accum = buildSamples(Seq.empty)
      val grouped = accum.groupBy(identity).mapValues(_.size)
      
      assertResult(config.mixPayloadSize * iterations)(accum.size)
      println(grouped)
      
      val expected = config.mixPayloadSize * iterations / 4
      val tol = config.mixPayloadSize * iterations / 10
      grouped.getOrElse(scored1, 0) should be(expected +- tol)
      grouped.getOrElse(scored2, 0) should be(expected +- tol)
      grouped.getOrElse(scored3, 0) should be(expected +- tol)
      grouped.getOrElse(scored4, 0) should be(expected +- tol)
    }
  }
}