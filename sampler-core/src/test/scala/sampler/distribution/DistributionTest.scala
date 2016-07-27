package sampler.distribution

import scala.annotation.migration
import scala.collection.immutable.ListMap

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import sampler._
import sampler.maths._
import sampler.maths.Random;

class DistributionTest 
    extends FreeSpec 
    with GeneratorDrivenPropertyChecks 
    with Matchers {
  
	val oneToThousandGen = Gen.choose(1,1000)
	val intSetGen = Gen.containerOf[Set,Int](oneToThousandGen)
  val weightsTableGen = {
    val weightedStringTuple = 
      for{
        value <- Gen.listOfN(5, Gen.alphaChar).map(_.mkString)
        weight <- arbitrary[Double].retryUntil(_ > 0.1)
      } yield value -> weight
      
    for{
      size <- Gen.choose(1, 100)
      tuples <- Gen.listOfN(size, weightedStringTuple)
    } yield ListMap(tuples: _*)
  }
  
  val realRandom = Random
  val dummyRandom = new Random{
    def nextInt(n: Int): Int = throw new Exception
    def nextDouble(): Double = throw new Exception
  }
  def mockRandom = new Random{
	  def nextInt(n: Int) = 0
		val it = (1 to 99).map(_ / 100.0).toIterator
		def nextDouble() = it.next
  }
  
  "From weights table" in forAll(weightsTableGen){ wtTable =>
    whenever(wtTable.values.exists(_ > 0)){
      val actual = {
        wtTable.toDistribution
          .until(_.size == wtTable.size)
          .sample(mockRandom)
      }
    
      val expected = {
        val weights = wtTable.values.toIndexedSeq
        val totalWeight = weights.sum
        val indexedValues = wtTable.keys.toIndexedSeq
        val at = new AliasTable(weights.map(_ / totalWeight))
  
        (1 to wtTable.size).map{_ => 
          indexedValues(at.next(mockRandom))
        }
      }
      
      assert(actual === expected)
    }
  }
  
  "From IndexedSeq" in pending
  "From Function" in pending
  
  "Samplable filtering" in {
    forAll(intSetGen, intSetGen){ (pool: Set[Int], reject: Set[Int]) =>
      whenever(pool.size > 0){
        val filteredSamples = CommonDistributions.uniform(pool.toIndexedSeq)
          .filter{v => !reject.contains(v)}
          .until(_.size == 100)
          .sample(realRandom)
          .toSet
        assert(filteredSamples.intersect(reject).size === 0)
      }
    }
  }
  
  "Samplable until" in forAll(oneToThousandGen){ (n: Int) =>
    whenever(n > 0 && n <= 1000){
      assertResult(List.fill(n)(1)){
        CommonDistributions.always(1)
          .until(_.size == n)
          .sample(dummyRandom)
      }
    }
  }
  "Samplable combining" in {
    def makeIt(s: Seq[Int]) = Stream.continually(s).flatten.iterator
    def makeDist(s: Seq[Int]) = {
  		val it = makeIt(s)
  		Distribution.from(_ => it.next)
    }
    val operators = Gen.oneOf(
      (a: Int, b: Int) => a + b,
      (a: Int, b: Int) => a * b,
      (a: Int, b: Int) => Math.max(a, b)
    )
    forAll(arbitrary[Seq[Int]],arbitrary[Seq[Int]],operators){(seqA: Seq[Int], seqB: Seq[Int], operator: (Int, Int) => Int) =>
      whenever(seqA.size > 0 && seqB.size > 0){
        val aLength = seqA.size
        val result = makeDist(seqA).combine(makeDist(seqB))(operator)
          .until(_.size == aLength)
          .sample(dummyRandom)
        val expected = makeIt(seqA).zip(makeIt(seqB))
          .map{case (a,b) => operator(a,b)}
          .take(aLength)
          .toSeq
        assert(result == expected)
      }
    }
  }
  
  "Monad flatMap" in {
    val ten = 10
    val indexGen = Gen.listOfN(ten, Gen.choose(0, 9))
    val matrixGen = Gen.listOfN(ten, Gen.listOfN(ten, Gen.alphaChar))
    def dist[T](seq: Seq[T]) = {
      val it = seq.iterator
      Distribution.from { _ => it.next }
    }
    
    forAll(indexGen, matrixGen){(index: Seq[Int], matrix: Seq[Seq[Char]]) => 
      whenever(index.size > 0 && matrix.size > 0 && !matrix.exists(_.size == 0)){
        val indexDist: Distribution[Int] = dist(index)
        val distSeq: Seq[Distribution[Char]] = matrix.map(dist)
        
        val result = indexDist.flatMap{i => distSeq(i % distSeq.size)}
            .until(_.size == index.size)
            .sample(dummyRandom)
        
        val expected = {
         	val matrixDist1: Seq[Distribution[Char]] = matrix.map(dist)
          index.map{i => matrixDist1(i % distSeq.size).sample(dummyRandom)}
            .take(index.size)
        }
          
        assert(result === expected)
      }
    }
  }
  
  "Monad pure" in pending
}