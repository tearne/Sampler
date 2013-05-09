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

package sampler.abc

import scala.annotation.tailrec
import sampler.data.Empirical._
import sampler.math.Probability
import sampler.data.SerialSampleBuilder
import sampler.data.Empirical
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import sampler.math.Random
import sampler.run.Aborter
import sampler.run.UserInitiatedAbortException
import scala.util.Try
import org.slf4j.LoggerFactory
import sampler.run.LocalJobRunner
import sampler.run.Abortable
import scala.util.Failure
import scala.util.Success
import sampler.run.ActorJobRunner
import sampler.run.ActorJob
import sampler.run.akka.client.Runner
import sampler.run.WrappedAborter
import scala.language.existentials
import sampler.run.akka.worker.NodeApp
import sampler.run.akka.worker.RunnerFactory

object ABCUtil{
	def generateParticles(model: ABCModel)(
			samplablePop: Empirical[model.Parameters],
			quantity: Int, 
			tolerance: Double,
			aborter: Aborter
	): model.Population = {
		import model._
		@tailrec
		def nextParticle(failures: Int = 0): Particle[Parameters] = {
			if(aborter.isAborted) throw new UserInitiatedAbortException("Abort flag was set")
			else if(failures >= meta.particleRetries) throw new UserInitiatedAbortException(s"Aborted after the maximum of $failures trials")
			else{
				def getScores(params: Parameters): IndexedSeq[Double] = {
					val modelWithMetric = samplableModel(params, observations).map(_.distanceTo(observations))
					val modelWithScores = SerialSampleBuilder(modelWithMetric)(_.size == meta.reps)
						.filter(_ <= tolerance)
					modelWithScores
				}
				
				def getWeight(params: Parameters, numPassed: Int): Option[Double] = {
					val fHat = numPassed.toDouble / meta.reps
					val numerator = fHat * prior.density(params)
					val denominator = samplablePop.probabilities.map{case (params0, probability) => 
						probability.value * params0.perturbDensity(params)
					}.sum
					if(numerator > 0 && denominator > 0) Some(numerator / denominator)
					else None	
				}
				
				val res: Option[Particle[Parameters]] = for{
					params <- Some(samplablePop.sample().perturb()) if prior.density(params) > 0
					fitScores <- Some(getScores(params))
					weight <- getWeight(params, fitScores.size) 
				} yield(Particle(params, weight, fitScores.min))
				
				res match {
					case Some(p: Particle[Parameters]) => p
					case None => nextParticle(failures + 1)
				}
			}
		}
		
		(1 to quantity).map(i => nextParticle()) 
	}
	
	trait Tasker{
		def run(model: ABCModel)(
				pop: Empirical[model.Parameters], 
				jobSizes: Seq[Int], 
				tolerance: Double
		): Seq[Try[model.Population]]
	}
	
	class LocalTasker(runner: LocalJobRunner) extends Tasker{
		def run(model: ABCModel)(pop: Empirical[model.Parameters], jobSizes: Seq[Int], tolerance: Double): Seq[Try[model.Population]] = {
			val jobs = jobSizes.map{quantity => Abortable{aborter =>
				generateParticles(model)(pop, quantity, tolerance, aborter)
			}}
			
			runner.apply(jobs)
		}
	}
	
	case class ABCJob(samplablepopulation: Empirical[_], quantity: Int, tolerance: Double) extends ActorJob[ABCModel#Population]
	
	class ActorTasker(runner: ActorJobRunner) extends Tasker{
		def run(model: ABCModel)(pop: Empirical[model.Parameters], jobSizes: Seq[Int], tolerance: Double): Seq[Try[model.Population]] = {
			val jobs = jobSizes.map{quantity =>
				ABCJob(pop, quantity, tolerance)
			}
			
			//TODO Is there a way to eliminate this cast? 
			//It's needed since the ABCActorJob[T] type T can't
			//carry the model as required for model.Population
			runner(jobs).asInstanceOf[Seq[Try[model.Population]]]
		}
	}
	
	def startWorkerNode(model: ABCModel){
		new NodeApp(new ActorRunnerFactory(model))
	}
	
	class ActorRunnerFactory(model: ABCModel) extends RunnerFactory{
		def create = new ActorRunner(model)
			
		class ActorRunner(model: ABCModel) extends Runner{
			def run = PartialFunction[Any, Any]{
				case ABCJob(pop, quantity, tol) => 
					generateParticles(model)(
							pop.asInstanceOf[Empirical[model.Parameters]], 
							quantity, 
							tol, 
							new WrappedAborter(aborted)
					)
			}
		}
	}
}

class ABCMethod[M <: ABCModel](val model: M) extends Serializable{
	import model._
	val log = LoggerFactory.getLogger(this.getClass)
	
	def init: Population = {
		val numParticles = model.meta.numParticles
		(1 to numParticles).par.map(i => Particle(model.prior.sample(), 1.0, Double.MaxValue)).seq
	}
	
	def evolveOnce(
		  pop: Population, 
			tasker: ABCUtil.Tasker,
			tolerance: Double
	)(implicit r: Random): Option[Population] = {
		import model.meta
		
		// Number of particles to be generated per job?
		val jobSizes = (1 to meta.numParticles)
			.grouped(meta.particleChunking)
			.map(_.size).toList
		log.info(s"Tolerance = $tolerance, Job sizes = $jobSizes")
		
		// Prepare samplable Parameters from current population
		val population: Empirical[Parameters] = pop.groupBy(_.value).map{case (k,v) => (k,v.map(_.weight).sum)}.toEmpiricalWeighted
		
//		val jobs = jobSizes.map(numParticles => 
//			tasker.buildJob(population, numPaticles, tolerance) 
//		}).toList
//		val runnerResults: Seq[Try[Population]] = tasker.run(jobs)
//		
		val results: Seq[Try[Population]] = tasker.run(model)(population, jobSizes, tolerance)
		
		//TOD check correct number of results?
	    if(results.contains(Failure)) None 
	    else Some(results.collect{case Success(s) => s}.flatten)
	}
		
	def run(
			pop: Population, 
			tasker: ABCUtil.Tasker
	)(implicit r: Random): Option[Population] = {
		import model.meta
		
		@tailrec
		def refine(
				pop: Population, 
				numAttempts: Int, 
				currentTolerance: Double,
				previousTolerance: Double
		): Option[Population] = {
			log.info(numAttempts + " generations remaining")
			//TODO report a failure ratio at the end of a generation
			if(numAttempts == 0) Some(pop)
			else{
				evolveOnce(pop, tasker, currentTolerance) match {
					case None =>
						log.warn(s"Failed to refine current population, evolving within previous tolerance $previousTolerance")
						refine(pop, numAttempts - 1, previousTolerance, previousTolerance)
					case Some(newPop) =>
						//Next tolerance is the median of the previous best for each particle
						val fit = newPop.map(_.bestFit)
						val medianTolerance = model.statistics.quantile(newPop.map(_.bestFit).toEmpiricalSeq, Probability(0.5))
						refine(newPop, numAttempts - 1, medianTolerance, currentTolerance)
				}
			}
		}
		
		refine(pop, meta.refinements, meta.tolerance,  meta.tolerance)
	}
}
