package sampler.examples

import org.apache.commons.math3.distribution.BetaDistribution
import scala.annotation.tailrec
import scala.collection.GenSeq
import sampler.data.EmpiricalMetricComponent
import sampler.data.Empirical._
import sampler.data.Samplable
import sampler.math.Probability
import sampler.data.ParallelSampleBuilder
import sampler.math.Random

object CompetingUncertainty extends App with EmpiricalMetricComponent {
	val bA = new BetaDistribution(100, 5)
	val seA = (1 to 100000).map(i => bA.sample()).toEmpiricalSeq
	
	val bB = new BetaDistribution(150, 20)
	val seB = (1 to 100000).map(i => bB.sample()).toEmpiricalSeq
	
	// Metaparameters
	val requiredConf = 0.95
	val chunkSize = 2000
	implicit val random = new Random
	
	case class Sensitivity(testA: Double, testB: Double)
	val bothSe = new Samplable[Sensitivity, Random]{
		def sample(implicit r: Random) = {
			Sensitivity(seA.sample(r), seB.sample(r))
		}
	}
  
	val testASampleSize = 5
	val testBSampleSize = 2
    
	calculateConfidence(bothSe, testASampleSize, testBSampleSize)
  
	
  // Analysis code
  def calculateConfidence(sensitivityDist: Samplable[Sensitivity, Random], numFaecal: Int, numDust: Int) = {
      // Termination condition for drawing samples
      def terminateCondition(soFar: GenSeq[Boolean]) = {
    	val distance = metric.max(
    	    soFar.toEmpiricalTable, 
    	    soFar.take(soFar.size - chunkSize).toEmpiricalTable
    	)
    	
    	(distance < 0.0001) || (soFar.size > 1e8)
      }
      
      // The proportion of samples so far that are positive
      def proportionPositive(samples: GenSeq[Boolean]) = 
        samples.toEmpiricalTable.probabilities(true).value
      
      // Build model for result of 'numTrials',
      //incorporating uncertainty in test performance
      val detectionProbs = probDetection(sensitivityDist, numFaecal, numDust)
      val model = Samplable.bernouliTrial(detectionProbs)
      
      // Build sampling distribution
      val samples = new ParallelSampleBuilder(chunkSize)(model)(terminateCondition)
      
      // return proportion positive (i.e. confidence)
      proportionPositive(samples)
  }
  
  // Single trial Se => Multiple trials Se
  def probDetection(sensitivityDist: Samplable[Sensitivity, Random], numTestA: Int, numTestB: Int) = {
	  def probDetection(se: Sensitivity) = 1 - math.pow((1 - se.testA), numTestA) * math.pow((1 - se.testB), numTestB)
			  
	  sensitivityDist map (se => Probability(probDetection(se)))
  }
}