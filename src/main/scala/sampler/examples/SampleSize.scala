package sampler.examples

import sampler.data._
import sampler.data.Empirical._
import sampler.math.{Random, Probability}
import scala.collection.GenSeq
import sampler.data.Types.Column
import scala.annotation.tailrec
import sampler.io.CSVTableWriter
import sampler.r.ScriptRunner
import java.nio.file.{Paths, Files}

object SampleSize extends App with EmpiricalMetricComponent{
	//Domain parameters
	val populationSize = 100
	val sampleSize = 10
	
	val chunkSize = 2000
	val convergenceTolerance = 0.001
	implicit val random = new Random
	val outputPath = Paths.get("examples", "sampleSize")
	Files.createDirectories(outputPath)
	
	val prevalenceOfInterest = List(5, 10, 20, 30, 50, 70, 90)
	
	def numPositivesDistribution(numPositive: Int, populationSize: Int, sampleSize: Int): Samplable[Int, Random] = {
		val population = (1 to populationSize).map(_ <= numPositive)
		val model = Samplable.withoutReplacement(population, sampleSize)// Start with base model
				.map(_.count(identity))// / sampleSize.toDouble)		// Transform to model of sample prevalance
		model
	}
	
	def terminationCondition(soFar: GenSeq[Int]) = {
		val distance = metric.max(
			soFar.take(soFar.size - chunkSize).toEmpiricalTable, 
			soFar.toEmpiricalTable
		)
		  
		(distance < convergenceTolerance) || (soFar.size > 1e8)
	  }
//	
//	val samplingDistributions: Map[Int, EmpiricalTable[Int]] = prevalenceOfInterest.map{trueNumPositives => 
//	  	println(trueNumPositives)
//		
//		val model = numPositivesDistribution(trueNumPositives, populationSize, sampleSize)
//	  
//	  	// Sample the model until convergence
//	  	val builder = new ParallelSampleBuilder(chunkSize)
//	  	val dist = builder(model)(terminationCondition _).toEmpiricalTable
//	  	(trueNumPositives, dist)
//	}.toMap
//	
//	
//	val potentialPrevObservations =  (0 to populationSize).map(_.toDouble / populationSize)
//	val dataColumns = prevalenceOfInterest.map{p =>
//		@tailrec
//		def addZeroIfMissing(map: Map[Int, Double], keyRange: Seq[Int]): Map[Int, Double] = {
//			if(keyRange.size == 0) map
//			else{
//				val newMap = if(!map.contains(keyRange.head)) map + (keyRange.head -> 0.0) else map
//				addZeroIfMissing(newMap, keyRange.tail)
//			}
//		}
//		
//		val dist = addZeroIfMissing(
//				samplingDistributions(p).probabilities.mapValues(_.value),
//				0 to populationSize
//		).mapValues(_.toDouble / sampleSize)
//		
//		Column(
//			(0 to sampleSize).map(possiblePositives => dist(possiblePositives)), 
//			p.toString
//		)
//	}
//	val t = Column((0 to sampleSize).map(_.toDouble / sampleSize), "ObsPrev")
//	val a = t :: dataColumns
//	
//	new CSVTableWriter(outputPath.resolve("output.csv"), true)(a: _*)
	
	val plotScript = """
require("ggplot2")
require("reshape")

data <- read.csv("output.csv")

names(data) <- c("ObsPrev", "5", "10", "20", "30", "50", "70", "90")

pdf("sampleSize.pdf", width=4.13, height=2.91) #A7 landscape paper)
ggplot(melt(data,id="ObsPrev"), aes(x=ObsPrev, y=value, colour=variable)) +  
    geom_line() + 
    scale_x_continuous(name="Observed Prevalence") +
    scale_y_continuous(name="Density") + 
    scale_colour_discrete(name="True Prev (%)") + 
	opts(title = "title")
dev.off()
"""

	//ScriptRunner(plotScript, outputPath.resolve("plotScript.r"))

	//
	// Now how many samples to be 95% confident in hitting true prev 
	//
	
	val confidence = Probability(0.95)
	val precision = 0.15
	val biggestPossibleSample = populationSize
	
	def minimumSampleSize(truNumPos: Int): Int = {
		def samplingDistribution(trueNumPos: Int, popSize: Int, sampSize: Int) = {
			val model = numPositivesDistribution(trueNumPos, popSize, sampSize)
			
		  	// Sample the model until convergence
		  	val builder = new ParallelSampleBuilder(chunkSize)
		  	val dist = builder(model)(terminationCondition _).toEmpiricalTable
		  	val obsDist = dist.counts.map{case (k,v) => (k.toDouble / sampSize, v)}.toEmpiricalTable
		  	//println(t.probabilities)
		  	obsDist
		}
		
		val reqSampleSize = (1 to biggestPossibleSample)
			.view
			.map{sampleSize =>
				println("trying "+sampleSize)
				val sampDist = samplingDistribution(truNumPos, populationSize, sampleSize)
				val lowerTail = sampDist.quantile(Probability(0.025))
				val upperTail = sampDist.quantile(Probability(0.975))
				val truePrev = truNumPos.toDouble / populationSize
				val lowerAcceptibleError = truePrev - precision
				val upperAcceptibleError = truePrev + precision
				println(s"$lowerAcceptibleError, $lowerTail, $upperTail, $upperAcceptibleError")
				val acceptibleConfidence = lowerAcceptibleError <= lowerTail && upperTail <= upperAcceptibleError 
				(sampleSize, acceptibleConfidence)
			}
			.find(_._2)
			.get._1
		
		println(s"Sample size is $reqSampleSize")
		reqSampleSize
	}
	
	def analyticalSampleSize(truNumPos: Int) = {
		val prev = truNumPos.toDouble / populationSize
		val size = (math.pow(1.96,2) * prev * (1-prev)*(100 - 10)/(99))/math.pow(0.09,2)
		size
	}
	
	val possibleTruePositives = List(10,30,50,70,90)
	val sampleSizes = possibleTruePositives.map{trueNumPos =>
		minimumSampleSize(trueNumPos)
	}
	val analyticalSampleSizes = possibleTruePositives.map{trueNumPos =>
		analyticalSampleSize(trueNumPos)
	}
	
	(possibleTruePositives zip analyticalSampleSizes).foreach(println)
	
	new CSVTableWriter(outputPath.resolve("sampleSizes.csv"), true)(
		Column(possibleTruePositives, "TruePos"),
		Column(sampleSizes, "Sampled"),
		Column(analyticalSampleSizes, "Analytical")
	)
	
	val sampleSizeScript = """
require("ggplot2")
require("reshape")

data <- read.csv("sampleSizes.csv")

#names(data) <- c("ObsPrev", "5", "10", "20", "30", "50", "70", "90")

pdf("sampleSize.pdf", width=4.13, height=2.91) #A7 landscape paper
ggplot(melt(data,id="TruePos"), aes(x=TruePos, y=value, colour=variable)) +  
    geom_line() + 
#    scale_x_continuous(name="Observed Prevalence") +
#    scale_y_continuous(name="Density") + 
#    scale_colour_discrete(name="True Prev (%)") + 
	opts(title = "title")
dev.off()
"""
		
		ScriptRunner(sampleSizeScript, outputPath.resolve("sampleSizeScript.r"))
}