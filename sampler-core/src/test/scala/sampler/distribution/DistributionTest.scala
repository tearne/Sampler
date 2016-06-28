package sampler.distribution

import sampler._
import org.scalatest.FreeSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import sampler.math.Random
import org.scalatest.Matchers
import org.scalatest.prop.Checkers
import org.scalacheck.Gen

class DistributionTest 
    extends FreeSpec 
    with GeneratorDrivenPropertyChecks 
//    with Checkers
    with Matchers {
  
  "A distribution" - {
    implicit val r = Random
    
    "Is a Samplable" - {
      val oneToHundred = Gen.choose(1,1000)
      val intSet = Gen.containerOf[Set,Int](oneToHundred)
      
      "Filtering" in forAll(intSet, intSet){ (pool: Set[Int], reject: Set[Int]) =>
        whenever(pool.size > 0){
          val filteredSamples = CommonDistributions.uniform(pool.toIndexedSeq)
            .filter{v => !reject.contains(v)}
            .until(_.size == 100)
            .sample
            .toSet
          assert(filteredSamples.intersect(reject).size === 0)
        }
      }
      
      "Until" in forAll(oneToHundred){ (n: Int) =>
        assertResult(List.fill(n)(1)){
          CommonDistributions.always(1)
            .until(_.size == n)
            .sample
        }
      }
      
      "Combining" in {
        def makeIt(s: Seq[Int]) = Stream.continually(s).flatten.iterator
        def makeDist(s: Seq[Int]) = {
      		val it = makeIt(s)
      		Distribution.from(_ => it.next)
        }
        forAll{(seqA: Seq[Int], seqB: Seq[Int]) =>
          whenever(seqA.size > 0 && seqB.size > 0){
            val aLength = seqA.size
            val result = makeDist(seqA).combine(makeDist(seqB))(_ + _)
              .until(_.size == aLength)
              .sample
            val expected = makeIt(seqA).zip(makeIt(seqB))
              .map{case (a,b) => a + b}
              .take(aLength)
              .toSeq
            assert(result == expected)
          }
        }
      }
      
      "Extends Monad" in {
        pending
      }
    }
  }
}