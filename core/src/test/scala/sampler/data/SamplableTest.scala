package sampler.data

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Before
import org.junit.Test
import sampler.math.Probability
import sampler.math.Random
import org.scalatest.matchers.ShouldMatchers
import sampler.math.Partition

class SamplableTest extends AssertionsForJUnit with ShouldMatchers {

  var instance: Samplable[Int] = _
  var instance2: Samplable[Int] = _
  var alwaysOne: Samplable[Int] = _
  implicit var random: Random = _
  
  @Before def initialise {
    random = Random
    
    instance = new Samplable[Int] {
	  val it = List(0,1,2,3,4,5,6,7,8,9).iterator
			
	  def sample(): Int = it.next()
	}
    
    instance2 = new Samplable[Int] {
      val it = List(0,1,2,3,4,5,6,7,8,9).iterator
      
      def sample(): Int = it.next()
    }
    
    alwaysOne = new Samplable[Int] {
      def sample = 1
    }
  }
  
  def append(previous: Seq[Int], s: Samplable[Int], requiredLength: Int): Seq[Int] = {
    if(previous.length == requiredLength) previous
	else append(previous.:+(s.sample()), s, requiredLength)
  }
  
  @Test def samplesValuesInOrder {
    val sampledSeq = append(Seq(), instance, 10)
			
	assert(sampledSeq === Seq(0,1,2,3,4,5,6,7,8,9))
  }
  
  @Test def sampleUntilLengthFive {
    val resultsSeq = instance.until(_.size == 5).sample
				
	val expectedSeq = IndexedSeq(0,1,2,3,4)
				
	assert(resultsSeq === expectedSeq)
  }
  
  @Test def sampleRepeatedlyUsingUntilEvenValue {
    val untilInstance = instance.until(_.last % 2 == 0)
				
	val seq1 = untilInstance.sample
	val seq2 = untilInstance.sample
	val seq3 = untilInstance.sample
				
	assert(seq1 === IndexedSeq(0))
	assert(seq2 === IndexedSeq(1,2))
	assert(seq3 === IndexedSeq(3,4))
  }
  
  @Test def filterForValuesGreaterThanTwo {
    val filtered = instance.filter(_ > 2)
	
    val sampleList = append(Seq(filtered.sample()), filtered, 7)
				
	assert(sampleList === Seq(3,4,5,6,7,8,9))
  }
  
  @Test def filtersForEvenNumbers {
    val filtered = instance.filter(_ % 2 == 0)
    
    val sampleList = append(Seq(), filtered, 5)
				
	assert(sampleList === Seq(0,2,4,6,8))
  }
  
  @Test def mapsValuesToDoubleValue {
    val mapped = instance.map(value => value * 2)
    
	val sampleList = append(Seq(), mapped, 10)
			
	assert(sampleList === List(0,2,4,6,8,10,12,14,16,18))
  }
  
  @Test def flatMap {
    // TODO
  }
  
  @Test def combinedTwoSamplablesWithProduct {
    def product(a: Int, b: Int) = a*b
		    
    val result = instance.combine(instance2)(product)
    
	val sampleList = append(Seq(), result, 5)
				
	assert(sampleList === Seq(0,1,4,9,16))
  }
  
  @Test def addTwoSamplablesWithConvolvle {
    val result = instance.convolve(alwaysOne)
	
    val sampleList = append(Seq(), result, 5)
				
	assert(sampleList === Seq(1,2,3,4,5))
  }
  
  @Test def subtractSamplableWithCrossCorrelate {
    val result = instance.crossCorrelate(alwaysOne)
		    
	val sampleList = append(Seq(), result, 5)
				
	assert(sampleList === Seq(-1,0,1,2,3))
  }
  
  @Test def continuallyAlwaysGivesSameResult {
    val model1 = Samplable.continually(1)
    val model2 = Samplable.continually(2)
    
    val r1 = (1 to 10).map(_ => model1.sample)
    val r2 = (1 to 10).map(_ => model2.sample)
    
    assert(r1.sum === 10)
    assert(r2.sum === 20)
  }
  
  @Test def uniformDistributionWithDoubleParameters {
    val model = Samplable.uniform(0.5, 1.5)
    
    val results = (1 to 10000).map(_ => model.sample)
    
    assert(results.count(_ >= 0.4) === 10000)
    results.count(_ >= 1.4) should be (1000 plusOrMinus 200)
    results.count(_ <= 0.6) should be (1000 plusOrMinus 200)
  }

  @Test def uniformDistributionWithIntParameters {
    val model = Samplable.uniform(1, 11)		//excludes 11
    
    val results = (1 to 10000).map(_ => model.sample)
    
    results.count(_ == 1) should be (1000 plusOrMinus 200)
    results.count(_ == 2) should be (1000 plusOrMinus 200)
    results.count(_ == 3) should be (1000 plusOrMinus 200)
    results.count(_ == 4) should be (1000 plusOrMinus 200)
    results.count(_ == 5) should be (1000 plusOrMinus 200)
    results.count(_ == 6) should be (1000 plusOrMinus 200)
    results.count(_ == 7) should be (1000 plusOrMinus 200)
    results.count(_ == 8) should be (1000 plusOrMinus 200)
    results.count(_ == 9) should be (1000 plusOrMinus 200)
    results.count(_ == 10) should be (1000 plusOrMinus 200)
  }

  @Test def iterableUniform {
    val items = IndexedSeq(2,4,6,8)
    
    val model = Samplable.uniform(items)
    
    val results = (1 to 2000).map(_ => model.sample)
    
    results.count(_ == 2) should be (500 plusOrMinus 100)
    results.count(_ == 4) should be (500 plusOrMinus 100)
    results.count(_ == 6) should be (500 plusOrMinus 100)
  }

  @Test def withoutReplacementNeverSamplesSameObjectTwice {
    val items = IndexedSeq(2,4,6,8)
    
    val model = Samplable.withoutReplacement(items, 2)
    
    def theSame(l: List[Int]) = if(l(0)	== l(1)) true else false
    
    val results = (1 to 100).map(_ => model.sample)
    
    assert(results.count(theSame(_) == true) === 0)
  }
  
  @Test def withoutReplacementDrawsWithEqualProbability {
    val it = IndexedSeq(2,4,6,8)
    
    val model = Samplable.withoutReplacement(it, 2)
    
    val results = (1 to 1000).map(_ => model.sample)
    
    results.count(_.contains(2)) should be (500 plusOrMinus 100)
    results.count(_.contains(4)) should be (500 plusOrMinus 100)
    results.count(_.contains(6)) should be (500 plusOrMinus 100)
  }
  
  @Test def sampleInfectedFromBinaryPopulation {
    val model = Samplable.binaryPopulation(5, 100)
    
    val results = (1 to 10000).map(_ => model.sample)
    
    results.count(_ == true) should be (500 plusOrMinus 100)
  }
  
  @Test def normalDistributionRegeneratesParameters {
    val definedMean = 5.0
    val definedSD = 1.5
    val model = Samplable.normal(definedMean, definedSD)
    
    val samples = (1 to 10000).map(_ => model.sample)
    
    val sampledMean = samples.sum / 10000
    
    def squaredDiff(d: Double) = {
      val sd = d - sampledMean
      sd*sd
    }
    
    val variance = (samples.map(s => squaredDiff(s))).sum/10000
    val sampledSD = Math.sqrt(variance)
    
    sampledMean should be (definedMean plusOrMinus 0.05)
    sampledSD should be (definedSD plusOrMinus 0.05)
  }
  
  @Test def bernouliTrialWithProbabilityOne {
    val model = Samplable.bernouliTrial(Probability(1))
    
    val result = (1 to 10).map(_ => model.sample)
    
    assert(result.count(_ == true) === 10)
  }
  
  @Test def bernoiliTrialWithProbabilityZero {
    val model = Samplable.bernouliTrial(Probability(0))
    
    val result = (1 to 10).map(_ => model.sample)
    
    assert(result.count(_ == true) === 0)
  }

  @Test def bernouliTrialWith80PercentProbability {
    val model = Samplable.bernouliTrial(Probability(0.8))
		    
	val result = (1 to 1000).map(_ => model.sample)
		    
	result.count(_ == true) should be (800 plusOrMinus 50)
  }
  
  @Test def coinTossIsFair {
    val model = Samplable.coinToss
		  
    val result = (1 to 1000).map(_ => model.sample)
		  
	result.count(_ == true) should be (500 plusOrMinus 50)
  }
  
  @Test def samplesFromItemsBasedOnPartitionProbabilities {
    val seq = IndexedSeq(1,2,3,4)
    val partition = new Partition(IndexedSeq(0.1,0.2,0.3,0.4).map(Probability(_)))
    
    val model = Samplable.fromPartition(seq, partition)
    
    val result = (1 to 1000).map(_ => model.sample)
    
    result.count(_ == 1) should be (100 plusOrMinus 50)
    result.count(_ == 2) should be (200 plusOrMinus 50)
    result.count(_ == 3) should be (300 plusOrMinus 50)
    result.count(_ == 4) should be (400 plusOrMinus 50)
  }
}