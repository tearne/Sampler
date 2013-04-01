/*
 * Copyright (c) 2012 Crown Copyright 
 *                    Animal Health and Veterinary Laboratories Agency
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package sampler.examples

import sampler.abc.ABCModel
import sampler.math._
import sampler.abc.ABCMeta
import sampler.data.Samplable
import sampler.abc.Prior
import sampler.abc.ABCMethod
import sampler.run.SerialRunner
import java.nio.file.{Paths, Files}
import sampler.data.Types.Column
import sampler.io.CSVTableWriter
import sampler.r.ScriptRunner
import org.apache.commons.math3.distribution.BetaDistribution
import sampler.run.cluster.Runner

object Recapture extends App{
	val wd = Paths.get("egout","recapture")
	Files.createDirectories(wd)

	/*
	 * Population size
	 */
	val abcMethod = new ABCMethod(RecaptureModel)
	implicit val abcRandomFactory = RandomFactory

	val encapPopulation0 = abcMethod.init
	
	val runner = new Runner
//	val runner = new SerialRunner
	
	val finalPopulation = abcMethod.run(encapPopulation0, runner).get.map(_.value)//.population
//	val finalPopulation = finalEncapPopulation.population//.map(_.value.asInstanceOf[RecaptureModel.Parameters])
	
	runner.shutdown
	
	new CSVTableWriter(wd.resolve("recapture.csv"), overwrite = true).apply(
		Column(finalPopulation.map(_.populationSize), "popSize"),
		Column(finalPopulation.map(_.prevalence), "prev")
	)

	
	val rScript = 
s"""
lapply(c("ggplot2", "reshape", "deSolve"), require, character.only=T)

recapture = read.csv("recapture.csv")

pdf("plots.pdf", width=4.13, height=2.91) #A7 landscape paper
ggplot(melt(data.frame(Se=rbeta(1000000,21,2), Sp=rbeta(1000000,42,1.8))), aes(x=value, colour=variable))+geom_density()
ggplot(subset(recapture, select="popSize"), aes(x=popSize)) + geom_histogram(binwidth=10)
ggplot(subset(recapture, select="prev"), aes(x=prev)) + geom_density()
dev.off()
"""

//	ScriptRunner.apply(rScript, wd.resolve("script.r"))
}

object RecaptureModel extends RecaptureModelBase {
  val abcRandomSource = RandomFactory
  val animalRandomSource = RandomFactory
}

//object TestRecaptureModel extends RecaptureModelBase {
//  val abcRandomSource = new RandomSourceImpl {}
//  val animalRandomSource = new RandomSourceImpl {
//    def newRandom = mock[Random]
//  }
//}

trait RecaptureModelBase extends ABCModel with Serializable{
	val statistics = new StatisticsComponentImpl {}

  implicit val abcRandomSource: RandomFactory //= new RandomRandomSource {} // Used implicitly ... check use sites
  val abcRandom = abcRandomSource.newRandom

	val animalRandomSource: RandomFactory // = new RandomRandomSource {}
  val animalRandom = animalRandomSource.newRandom    // Used explicitly ... check use sites
	
	val numberTagged = 50
	val observations = Observations(220, 35, 86.0/220)
	
	val se = new BetaDistribution(21, 2) 
	val sp = new BetaDistribution(42, 1.8)

    val meta = new ABCMeta(
    	reps = 10,
		numParticles = 200, 
		refinements = 30,
		particleRetries = 100, 
		particleChunking = 10
	)
	
    case class Parameters(populationSize: Int, prevalence: Double) extends ParametersBase with Serializable{
      val kernel = Samplable.normal(0, 0.1)
	  val threeDie = Samplable.uniform(IndexedSeq(-1,0,0,0,1))
      private def threeDensity(v: Int) = v match{
      	case -1 => 1.0 / 5
      	case 1 => 1.0 / 5
      	case 0 => 3.0 / 5
      	case _ => 0
      }
		
      def perturb = Parameters(
      	populationSize + threeDie.sample,
      	kernel.sample + prevalence
      )
      def perturbDensity(that: Parameters) = 
      	threeDensity(populationSize - that.populationSize) *
      	kernel.density(prevalence - that.prevalence)
    }

    case class Observations(numSampled: Int, numRecaptured: Int, prevalence: Double) extends ObservationsBase with Serializable
    
    case class Output(obs: Observations) extends OutputBase with Serializable{
      def distanceTo(otherObs: Observations): Double = {
      	assert(obs.numSampled == otherObs.numSampled)
      	val d1 = math.abs(obs.numRecaptured - otherObs.numRecaptured)
      	val d2 = math.pow(obs.prevalence - otherObs.prevalence, 4.0)
      	d1 + d2
      }
    }
    

    case class AnimalState(tagged: Boolean, infected: Boolean)
    def samplableModel(p: Parameters, obs: Observations) = {
		def numTaggedDistribution(numTagged: Int, populationSize: Int, sampleSize: Int): Samplable[Output] = {
    	  val population = (1 to populationSize).map(index => 
    	  	AnimalState(index <= numTagged, animalRandom.nextBoolean(Probability(p.prevalence)))
    	  )
    	  def getNumPositives(animals: Seq[AnimalState]): Int = {
    	  	val testResults = animals.map{a =>
    	  		if(a.infected) animalRandom.nextBoolean(Probability(se.sample))
    	  		else animalRandom.nextBoolean(Probability(1 - sp.sample))
    	  	}
    	  	testResults.count(identity)
    	  }
    	  
    	  val model = Samplable.withoutReplacement(population, sampleSize) // TODO is the the random source for the model or the 
                                                                         // meta-model? If the model then be explicit about (animalRandom)
    		.map{sampledStates =>
			  Output(Observations(
				sampleSize,
				sampledStates.map(_.tagged).count(identity),
				getNumPositives(sampledStates) / sampleSize.toDouble
		      ))
    	  	}
		  model
		}
    	
    	numTaggedDistribution(numberTagged, p.populationSize, obs.numSampled)
    }
    
    val prior = new Prior[Parameters] with Serializable{
    	val upperLimit = 500
    	val lowerLimit = 250
    	def unitRange(d: Double) = if(d > 1.0 || d < 0.0) 0.0 else 1.0
    	def popSizeRange(n: Int) = 	      
    		if(n > upperLimit || n < lowerLimit) 0.0
    		else 1.0 / (upperLimit - lowerLimit)
    	
    	def density(p: Parameters) = {
    		popSizeRange(p.populationSize) * unitRange(p.prevalence)
	    }
	    
	    def sample = Parameters(
	    		abcRandom.nextInt(upperLimit-lowerLimit) + lowerLimit,
	    		abcRandom.nextDouble(0, 1)
	    )
    }
}
