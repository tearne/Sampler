package sampler.abc.actor.root.phase.task.egen

import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FreeSpec, Matchers}
import sampler.abc.{ABCConfig, Scored, Weighted}
import sampler.maths.Random

class ParticleMixerTest extends FreeSpec with Matchers with MockitoSugar with BeforeAndAfter {

  "ParticleMixer should" - {
    val instance = new ParticleMixer()
    
    val config = new ABCConfig(null){
      override lazy val mixPayloadSize = 2
    }
    
    val scored1 = Scored(1, Seq(1,1,1,1))
    val scored2 = Scored(2, Seq(2,2,2,2))
    val scored3 = Scored(3, Seq(3,3,3,3))
    val scored4 = Scored(4, Seq(4,4,4,4))
    
    val weighed1 = Weighted(scored1, 0.25)
    val weighed2 = Weighted(scored2, 0.25)
    val weighed3 = Weighted(scored3, 0.25)
    val weighed4 = Weighted(scored4, 0.25)
    
    val irrelevant = 1
    
    "return None when no weighed particles present in generation" in {
      val eGen = mock[EvolvingGeneration[Int]]
      when(eGen.mixingPool).thenReturn(Seq.empty[Weighted[Int]])  
      
      val result = instance.apply(eGen, config)(null)
      assert(result === None)
    }
    
    "return all current weighed particles if fewer available than mixing size" in {      
      val eGen = mock[EvolvingGeneration[Int]]
      when(eGen.mixingPool).thenReturn(Seq(weighed1, weighed2))  
        
      val result = instance.apply(eGen, config)(null).get
      assert(result.seq == Seq(scored1, scored2))
    }
    
    "randomly select from weighted particles when more present than required" in {
      val eGen = mock[EvolvingGeneration[Int]]
      when(eGen.mixingPool).thenReturn(Seq(weighed1, weighed2, weighed3, weighed4))  
      val iterations = 10
        
      //TODO something clever with mock random or distriubtion builder?
      def buildSamples(acc: Seq[Scored[Int]], count:Int = 0, reps: Int = iterations): Seq[Scored[Int]] = {
        if(count >= reps) acc
        else buildSamples(acc ++ instance.apply(eGen, config)(Random).get.seq, count+1)
      }
      
      val accum = buildSamples(Seq.empty)
      val grouped = accum.groupBy(identity).mapValues(_.size)
      
      assertResult(config.mixPayloadSize * iterations)(accum.size)
      println(grouped)
      
      val expected = config.mixPayloadSize * iterations / 4
      val tol = config.mixPayloadSize * iterations / 9
      grouped.getOrElse(scored1, 0) should be(expected +- tol)
      grouped.getOrElse(scored2, 0) should be(expected +- tol)
      grouped.getOrElse(scored3, 0) should be(expected +- tol)
      grouped.getOrElse(scored4, 0) should be(expected +- tol)
    }
  }
}