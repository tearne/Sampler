package sampler.abc.actor.root.state.task.egen

import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FreeSpec, Matchers}
import sampler.abc.actor.message.ScoredParticles
import sampler.abc.{ABCConfig, Scored, Weighted}
import sampler.maths.Random

class ParticleMixerTest extends FreeSpec with Matchers with MockitoSugar with BeforeAndAfter {

  val scored1 = Scored(1, Seq(1,1,1,1))
  val scored2 = Scored(2, Seq(2,2,2,2))
  val scored3 = Scored(3, Seq(3,3,3,3))
  val scored4 = Scored(4, Seq(4,4,4,4))

  val weighed1 = Weighted(scored1, 0.25)
  val weighed2 = Weighted(scored2, 0.25)
  val weighed3 = Weighted(scored3, 0.25)

  //Duplicates, could happen if same particle sampled several times and gets same scores
  val weighed4a = Weighted(scored4, 0.25)
  val weighed4b = Weighted(scored4, 0.25)
  val weighed4c = Weighted(scored4, 0.25)

  val config = new ABCConfig(null){
    override lazy val mixPayloadSize = 2
  }

  trait MockedSamplingInstance {
    val instance = new ParticleMixer with ParticleMixerHelper {
      def drawMixParticles[P](number: Int, bag: Map[Scored[P], Int])(implicit r: Random): Seq[Scored[P]] = {
          assert(number === 2)
          assert(bag === Map(scored1 -> 1, scored2 -> 1, scored3 -> 1, scored4 -> 1))
          Seq(scored1, scored4).asInstanceOf[Seq[Scored[P]]]
      }
    }
  }

  trait RealInstance {
    val instance = ParticleMixer
  }

  "ParticleMixer should" - {
    "return None when no weighed particles present in generation" in new RealInstance {
      val eGen = mock[EvolvingGeneration[Int]]
      when(eGen.mixingPool).thenReturn(Seq.empty[Weighted[Int]])  
      
      val result = instance.apply(eGen, config)(null)
      assert(result === None)
    }
    
    "return all scored particles if fewer available than mixing size" in new RealInstance {
      val eGen = mock[EvolvingGeneration[Int]]
      when(eGen.mixingPool).thenReturn(Seq(weighed1))
        
      val result = instance.apply(eGen, config)(null).get
      assert(result.seq == Seq(scored1))
    }

    "return all scored partilces even if there _appear_ to be more prior to consolidation" in new RealInstance {
      val eGen = mock[EvolvingGeneration[Int]]
      when(eGen.mixingPool).thenReturn(Seq(weighed4a, weighed4b, weighed4c)) //Only 1 scored particle here

      val result = instance.apply(eGen, config)(null).get
      //assert(result.size === 1)
      assert(result.seq === Seq(scored4))
    }

    "sample required number of scored particles when excess available" in new MockedSamplingInstance {
      val eGen = mock[EvolvingGeneration[Int]]
      val weighted: Seq[Weighted[Int]] = Seq(weighed1, weighed2, weighed3, weighed4a, weighed4b, weighed4c)
      when(eGen.mixingPool).thenReturn(weighted)

      val result = instance.apply(eGen, config)(null)
      val expected = Option(ScoredParticles(Seq(scored1, scored4)))

      assert(result === expected)
    }
  }
}