package sampler.data

import org.junit.Before
import org.junit.Test
import sampler.math.Random
import sampler.math.Partition
import org.scalatest.Matchers
import org.scalatest.FreeSpec
import org.scalatest.BeforeAndAfter

class DistributionTest extends FreeSpec with Matchers with BeforeAndAfter {

  var instance: Distribution[Int] = _
  var instance2: Distribution[Int] = _
  val alwaysOne: Distribution[Int] = Distribution.continually(1)
  implicit val random: Random = Random
  
  before {
    instance = {
      val it = List(0,1,2,3,4,5,6,7,8,9).iterator
      Distribution(it.next)
    }
    
    instance2 = {
      val it = List(0,1,2,3,4,5,6,7,8,9).iterator
      Distribution(it.next)
    }
  }
  
  def append(previous: Seq[Int], d: Distribution[Int], requiredLength: Int): Seq[Int] = {
    if(previous.length == requiredLength) previous
	else append(previous.:+(d.sample()), d, requiredLength)
  }
  
  "samplesValuesInOrder" in {
    val sampledSeq = append(Seq(), instance, 10)
			
	assert(sampledSeq === Seq(0,1,2,3,4,5,6,7,8,9))
  }
  
  "sampleUntilLengthFive" in {
    val resultsSeq = instance.until(_.size == 5).sample
				
	val expectedSeq = IndexedSeq(0,1,2,3,4)
				
	assert(resultsSeq === expectedSeq)
  }
  
  "sampleRepeatedlyUsingUntilEvenValue" in {
    val untilInstance = instance.until(_.last % 2 == 0)
				
	val seq1 = untilInstance.sample
	val seq2 = untilInstance.sample
	val seq3 = untilInstance.sample
				
	assert(seq1 === IndexedSeq(0))
	assert(seq2 === IndexedSeq(1,2))
	assert(seq3 === IndexedSeq(3,4))
  }
  
  "filterForValuesGreaterThanTwo" in {
    val filtered = instance.filter(_ > 2)
	
    val sampleList = append(Seq(filtered.sample()), filtered, 7)
				
	assert(sampleList === Seq(3,4,5,6,7,8,9))
  }
  
  "filtersForEvenNumbers" in {
    val filtered = instance.filter(_ % 2 == 0)
    
    val sampleList = append(Seq(), filtered, 5)
				
	assert(sampleList === Seq(0,2,4,6,8))
  }
  
  "mapsValuesToDoubleValue" in {
    val mapped = instance.map(value => value * 2)
    
	val sampleList = append(Seq(), mapped, 10)
			
	assert(sampleList === List(0,2,4,6,8,10,12,14,16,18))
  }
  
  "flatMap" in {
    val flatMapped = instance.flatMap(x => Distribution.continually(x * 10))
    
    val sampleList = append(Seq(), flatMapped, 10)
    
    assert(sampleList === List(0,10,20,30,40,50,60,70,80,90))
  }
  
  "combinedTwoDistributionsWithProduct" in {
    def product(a: Int, b: Int) = a*b
		    
    val result = instance.combine(instance2)(product)
    
	val sampleList = append(Seq(), result, 5)
				
	assert(sampleList === Seq(0,1,4,9,16))
  }
  
  "addTwoDistributionsWithConvolvle" in {
    val result = instance.convolve(alwaysOne)
	
    val sampleList = append(Seq(), result, 5)
				
	assert(sampleList === Seq(1,2,3,4,5))
  }
  
  "subtractDistributionWithCrossCorrelate" in {
    val result = instance.crossCorrelate(alwaysOne)
		    
	val sampleList = append(Seq(), result, 5)
				
	assert(sampleList === Seq(-1,0,1,2,3))
  }
  
  "continuallyAlwaysGivesSameResult" in {
    val model1 = Distribution.continually(1)
    val model2 = Distribution.continually(2)
    
    val r1 = (1 to 10).map(_ => model1.sample)
    val r2 = (1 to 10).map(_ => model2.sample)
    
    assert(r1.sum === 10)
    assert(r2.sum === 20)
  }
  
  "uniformDistributionWithDoubleParameters" in {
    val model = Distribution.uniform(0.5, 1.5)
    
    val results = (1 to 10000).map(_ => model.sample)
    
    assert(results.count(_ >= 0.4) === 10000)
    results.count(_ >= 1.4) should be (1000 +- 200)
    results.count(_ <= 0.6) should be (1000 +- 200)
  }

  "uniformDistributionWithIntParameters" in {
    val model = Distribution.uniform(1, 11)		//excludes 11
    
    val results = (1 to 10000).map(_ => model.sample)
    
    results.count(_ == 1) should be (1000 +- 200)
    results.count(_ == 2) should be (1000 +- 200)
    results.count(_ == 3) should be (1000 +- 200)
    results.count(_ == 4) should be (1000 +- 200)
    results.count(_ == 5) should be (1000 +- 200)
    results.count(_ == 6) should be (1000 +- 200)
    results.count(_ == 7) should be (1000 +- 200)
    results.count(_ == 8) should be (1000 +- 200)
    results.count(_ == 9) should be (1000 +- 200)
    results.count(_ == 10) should be (1000 +- 200)
  }

  "iterableUniform" in {
    val items = IndexedSeq(2,4,6,8)
    
    val model = Distribution.uniform(items)
    
    val results = (1 to 2000).map(_ => model.sample)
    
    results.count(_ == 2) should be (500 +- 100)
    results.count(_ == 4) should be (500 +- 100)
    results.count(_ == 6) should be (500 +- 100)
  }

  "withoutReplacementNeverSamplesSameObjectTwice" in {
    val items = IndexedSeq(2,4,6,8)
    
    val model = Distribution.withoutReplacement(items, 2)
    
    def theSame(l: List[Int]) = if(l(0)	== l(1)) true else false
    
    val results = (1 to 100).map(_ => model.sample)
    
    assert(results.count(theSame(_) == true) === 0)
  }
  
  "withoutReplacementDrawsWithEqualProbability" in {
    val it = IndexedSeq(2,4,6,8)
    
    val model = Distribution.withoutReplacement(it, 2)
    
    val results = (1 to 1000).map(_ => model.sample)
    
    results.count(_.contains(2)) should be (500 +- 100)
    results.count(_.contains(4)) should be (500 +- 100)
    results.count(_.contains(6)) should be (500 +- 100)
  }
  
  "sampleInfectedFromBinaryPopulation" in {
    val model = Distribution.binaryPopulation(5, 100)
    
    val results = (1 to 10000).map(_ => model.sample)
    
    results.count(_ == true) should be (500 +- 100)
  }
  
  "bernoulliTrialWithProbabilityOne" in {
    val model = Distribution.bernoulliTrial(1)
    
    val result = (1 to 10).map(_ => model.sample)
    
    assert(result.count(_ == true) === 10)
  }
  
  "bernoilliTrialWithProbabilityZero" in {
    val model = Distribution.bernoulliTrial(0)
    
    val result = (1 to 10).map(_ => model.sample)
    
    assert(result.count(_ == true) === 0)
  }

  "bernoulliTrialWith80PercentProbability" in {
    val model = Distribution.bernoulliTrial(0.8)
		    
	val result = (1 to 1000).map(_ => model.sample)
		    
	result.count(_ == true) should be (800 +- 50)
  }
  
  "coinTossIsFair" in {
    val model = Distribution.coinToss
		  
    val result = (1 to 1000).map(_ => model.sample)
		  
	result.count(_ == true) should be (500 +- 50)
  }
  
  "buildingDistributionFromPartition" in {
    val seq = IndexedSeq(1,2,3,4)
    val partition = new Partition(IndexedSeq(0.1,0.2,0.3,0.4))
    
    val model = Distribution.fromPartition(seq, partition)
    
    val result = (1 to 1000).map(_ => model.sample)
    
    result.count(_ == 1) should be (100 +- 50)
    result.count(_ == 2) should be (200 +- 50)
    result.count(_ == 3) should be (300 +- 50)
    result.count(_ == 4) should be (400 +- 50)
  }
  
  "errorWhenPartitionLengthDoesntMatchSequenceLength" in {
    val seq = IndexedSeq(1,2,3,4)
    val partition = new Partition(IndexedSeq(0.25,0.25,0.5))
    
    intercept[AssertionError] {
      val model = Distribution.fromPartition(seq, partition)
    }
  }
  
  "buildingDistributionFromProbabilityTable" in {
    val probTable = Map(1 -> 0.1, 2 -> 0.2, 3 -> 0.3, 4 -> 0.4)
    
    val model = Distribution.fromProbabilityTable(probTable)
    
    val result = (1 to 1000).map(_ => model.sample)
    
    result.count(_ == 1) should be (100 +- 50)
    result.count(_ == 2) should be (200 +- 50)
    result.count(_ == 3) should be (300 +- 50)
    result.count(_ == 4) should be (400 +- 50)
  }
}