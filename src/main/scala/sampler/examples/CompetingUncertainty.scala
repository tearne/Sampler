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
import sampler.io.CSVTableWriter
import sampler.data.Types.Column
import sampler.r.ScriptRunner
import java.nio.file.{Paths, Files}

object CompetingUncertainty extends App with EmpiricalMetricComponent {
	val outputPath = Paths.get("examples", "competingUncertainty")
	Files.createDirectories(outputPath)
	
	
	/*
	 * 
	 * High performance:	95% > 0.9, mode = 0.98
	 * new BetaDistribution(42, 1.8) 
	 * 
	 * Med performance: 	95% > 0.8, mode = 0.95 
	 * new BetaDistribution(21, 2) 
	 * 
	 * Low performance: 	95% > 0.7, mode = 0.90
	 * new BetaDistribution(15, 2.5) 
	 */
	
	
	val trueASe = new BetaDistribution(21, 2) 
	val trueASp = new BetaDistribution(42, 1.8)
	val aSe = (1 to 100000).map(i => trueASe.sample()).toEmpiricalSeq
	val aSp = (1 to 100000).map(i => trueASp.sample()).toEmpiricalSeq
	val aCost = 50
	
	val trueBSe = new BetaDistribution(42, 1.8) 
	val trueBSp = new BetaDistribution(15, 2.5)
	val bSe = (1 to 100000).map(i => trueBSe.sample()).toEmpiricalSeq
	val bSp = (1 to 100000).map(i => trueBSp.sample()).toEmpiricalSeq
	val bCost = 10
	
	val aSeMedian = aSe.quantile(Probability(0.5))
	val aSpMedian = aSp.quantile(Probability(0.5))
	val bSeMedian = bSe.quantile(Probability(0.5))
	val bSpMedian = bSp.quantile(Probability(0.5))
	
	// Metaparameters
	val requiredConf = 0.95
	val chunkSize = 2000
	implicit val random = new Random
	
	case class Result(testResult: Boolean, wasInfected: Boolean, cost: Int)

	def screenedTestPoint(prev: Double): Result = {
		val isInfected = random.nextBoolean(Probability(prev))
		
		val firstTestResult = 
			if(isInfected) random.nextBoolean(Probability(bSeMedian))
			else random.nextBoolean(Probability(1 - bSpMedian))
		
		if(!firstTestResult) Result(false, isInfected, bCost)
		else{
			val secondTestResult = 
				if(isInfected) random.nextBoolean(Probability(aSeMedian))
				else random.nextBoolean(Probability(aSpMedian))
			Result(secondTestResult, isInfected, bCost + aCost)
		}
	}
	def screenedTestDist(prev: Double): Result = {
		val isInfected = random.nextBoolean(Probability(prev))
		
		val firstTestResult = 
			if(isInfected) random.nextBoolean(Probability(bSe.sample))
			else random.nextBoolean(Probability(1 - bSp.sample))
		
		if(!firstTestResult) Result(false, isInfected, bCost)
		else{
			val secondTestResult = 
				if(isInfected) random.nextBoolean(Probability(aSe.sample))
				else random.nextBoolean(Probability(aSp.sample))
			Result(secondTestResult, isInfected, bCost + aCost)
		}
	}
	
	case class SchemeResults(meanCost: Double, meanMissedInfections: Double)
	def getSe(prevalence: Double, testFunction: Double => Result) = {
		println(s"Working on $prevalence")
		val samplable = new Samplable[Result, Random]{
			def sample(implicit r: Random) = testFunction(prevalence)
		}.filter(_.wasInfected)
		def seEstimate(soFar: GenSeq[Result]) = {
			val numOpertunuties = soFar.size
			val numSuccess = soFar.filter(_.testResult).size
			numSuccess.toDouble / numOpertunuties
		}
		
		def terminationCondition(soFar: GenSeq[Result]) = {
			val distance = math.abs(
				seEstimate(soFar.take(soFar.size - chunkSize)) -
				seEstimate(soFar)
			)
			  
			(distance < 0.000001) || (soFar.size > 1e20)
		}
		
		val builder = new ParallelSampleBuilder(chunkSize)
		val dist = builder(samplable)(terminationCondition _)
//		
//		val meanCost = dist.map(_.cost).fold(0)(_ + _) / dist.size.toDouble
//		val meanMissedInfections = dist.map(result => result.wasInfected && result.testResult).count(identity) / dist.size.toDouble
//		
//		SchemeResults(meanCost, meanMissedInfections)
		seEstimate(dist)
	}
	
	val prevalance = (1 to 10).map(_ / 10.0)

	val seMedian = prevalance.map(p => getSe(p, screenedTestPoint))
	val seDist = prevalance.map(p => getSe(p, screenedTestDist))
	
//	val commonLength = math.min(medianBased.size,distBased.size)
//	val costMedian = medianBased.map(_.cost).take(commonLength)
//	val costDist = distBased.map(_.cost).take(commonLength)

	
//	println(seMedian)
//	println(seDist)
	
	
	new CSVTableWriter(outputPath.resolve("results.csv"), true)(
		Column(prevalance, "prevalance"),
		Column(seMedian, "seMedian"),
		Column(seDist, "seDist")
	)
	
	val plotScript = """
require("ggplot2")
require("reshape")

data <- read.csv("results.csv")

pdf("se.pdf", width=4.13, height=2.91) #A7 landscape paper)
ggplot(melt(data, id="prevalance"), aes(x=prevalance, y=value, colour=variable)) +  geom_line()
dev.off()
"""

	ScriptRunner(plotScript, outputPath.resolve("plotScript.r"))
	
//	println(getSchemeResults(0.4, singleTest))
//	println(getSchemeResults(0.4, screenedTest))
	
	
	
	
	/*
	
	val prevalances = (0 to 10).map(_ / 10.0)
	
	val singleTestScheme = prevalances.map{p => getSchemeResults(p, singleTest)}
	val singleTestCosts = singleTestScheme.map(_.meanCost)
	val singleTestMisses = singleTestScheme.map(_.meanMissedInfections)

	val screenedTestScheme = prevalances.map{p => getSchemeResults(p, screenedTest)}
	val screenedTestCosts = screenedTestScheme.map(_.meanCost)
	val screenedTestMisses = screenedTestScheme.map(_.meanMissedInfections)
	
	new CSVTableWriter(outputPath.resolve("results.csv"), true)(
		Column(prevalances, "prev"),
		Column(singleTestCosts, "singleCosts"),
		Column(screenedTestCosts, "screenedCosts"),
		Column(singleTestMisses, "singleMisses"),
		Column(screenedTestMisses, "screenedMisses")
	)
	
		val plotScript = """
require("ggplot2")
require("reshape")

data <- read.csv("results.csv")

pdf("costs.pdf", width=4.13, height=2.91) #A7 landscape paper)
ggplot(melt(subset(data, select=c("prev", "singleMisses", "screenedMisses")),id="prev"), aes(x=prev, y=value, colour=variable)) +  
    geom_line()
ggplot(melt(subset(data, select=c("prev", "singleCosts", "screenedCosts")),id="prev"), aes(x=prev, y=value, colour=variable)) +  
    geom_line()
dev.off()
"""

	ScriptRunner(plotScript, outputPath.resolve("plotScript.r"))
	*/
	
}