package sampler.run.cluster

import sampler.data.Samplable
import sampler.math.{Random, Probability}
import sampler.data.SampleBuilderComponent
import sampler.data.Empirical._
import scala.annotation.tailrec
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream

trait Prior[A,Rnd] extends Samplable[A,Rnd]{
	def density(value: A): Double
}
case class Particle[A](value: A, weight: Double, bestFit: Double)

trait ABCModel[R <: Random]{
	this: SampleBuilderComponent =>
	
	type Parameters <: ParametersBase
	protected trait ParametersBase {
		def perturb(random: R): Parameters
		def perturbDensity(that: Parameters): Double		
	}
	
	type Observations <: ObservationsBase
	protected trait ObservationsBase
	
	type Output <: OutputBase
	protected trait OutputBase {
		def distanceTo(observed: Observations): Double
	}
	
	def initModel(p: Parameters, obs: Observations): Samplable[Output,R]
	val prior: Prior[Parameters, R]
	val observations: Observations
	val abcParameters: ABCParameters
	
	def nextParticle(
			population: Seq[Particle[Parameters]],
			tolerance: Double,
			random: R
	) = {
		@tailrec
		def getParticle(failures: Int = 0): Option[Particle[Parameters]] = {
			//if(!keepGoing.get) None
			//else 
			if(failures >= abcParameters.particleRetries) None
			else{
				def getScores(params: Parameters) = {
					val metricModel = initModel(params, observations).map(_.distanceTo(observations))
					val metricScores = builder(metricModel)(_.size == abcParameters.reps)(random)
						.filter(_ <= tolerance)
					metricScores
				}
				
				def getWeight(params: Parameters, numPassed: Int) = {
					val fHat = numPassed.toDouble / abcParameters.reps
					val numerator = fHat * prior.density(params)
					val denominator = population.map{case Particle(value, weight, bestScore) => 
						weight * value.perturbDensity(params)
					}.sum
					if(numerator > 0 && denominator > 0) Some(numerator / denominator)
					else None	
				}
				
				//TODO inefficient to do this every time
				val samplable = population.groupBy(_.value).map{case (k,v) => (k,v.map(_.weight).sum)}.toEmpiricalWeighted
				
				val res = for{
					params <- Some(samplable.sample(random).perturb(random)) if prior.density(params) > 0
					fitScores <- Some(getScores(params))// if scores.size > 0
					weight <- getWeight(params, fitScores.size) 
				} yield(Particle(params, weight, fitScores.min))
				
				res match {
					case s: Some[Particle[Parameters]] => s
					case None => getParticle(failures + 1)
				}
			}
		}
		
		getParticle()
	}
}

abstract class EncapsulatedABC[R <: Random]{
	type E <: ABCModel[R]
	val env: E
	val prevPopulation: Seq[Particle[env.Parameters]]
}

object Encapsulator extends Serializable{
	def apply[R <: Random](abc0: ABCModel[R])(population0: Seq[Particle[abc0.Parameters]]) = new EncapsulatedABC[R] with Serializable{
		type E = abc0.type
		val env: E = abc0
		val prevPopulation = population0
	}
}

trait ABCComponent{
	this: SampleBuilderComponent =>
	
	def apply[R <: Random](
			model: ABCModel[R],
			runner: ClusterRunner,
			random: R
	): Seq[model.Parameters] = {
		type P = model.Parameters
		
		val uniformlyWeightedParticles = (1 to model.abcParameters.numParticles).par.map(i => Particle(model.prior.sample(random), 1.0, Double.MaxValue)).seq
		
		def evolve(population: Seq[Particle[P]], tolerance: Double): Option[Seq[Particle[P]]] = {
			import model._
			println("Now working on tolerance = "+tolerance)
			
			//Map the sequence of weighted params (particles) to a map from param to (summed) weight 
			val samplable = population.groupBy(_.value).map{case (k,v) => (k,v.map(_.weight).sum)}.toEmpiricalWeighted
			
			//How many particles generated per job?
			val jobSizes = (1 to abcParameters.numParticles)
				.grouped(abcParameters.particleChunking)
				.map(_.size).toList
			println(s"JobSizes: $jobSizes")
			
			//TODO JobRunner Abortable Job syntax too noisy
			val encap = Encapsulator.apply[R](model)(population)
			val jobs = jobSizes.map(numParticles => Job{() => 
				val particles = (1 to numParticles)
						.view
						.map(_ => encap.env.nextParticle(encap.prevPopulation, tolerance, random))
						.takeWhile(_.isDefined)
						.map(_.get)
						.force
					if(particles.size == numParticles) Some(particles) else None 
				}).toList
			val baos = new ByteArrayOutputStream()
			val oos = new ObjectOutputStream(baos)
			try{
				val tmp = oos.writeObject(jobs(0).f)
				println("object "+jobs(0).f)
				println("Object stream = "+tmp)
			}catch{
				case _ => throw new RuntimeException("Bleh!")
			}
			val results: Seq[Particle[P]] = runner{
					jobs
				}.view.takeWhile(_.isDefined).map(_.get).force.flatten
			
			if(results.size == abcParameters.numParticles) Some(results)
			else None
		}
		
		@tailrec
		def refine(population: Seq[Particle[P]], numAttempts: Int, tolerance: Double): Seq[Particle[P]] = {
			println("Generations left to go "+numAttempts)
			if(numAttempts == 0) population
			else{
				evolve(population, tolerance) match {
					case None =>
						println("Failed to refine current population, evolving within same tolerance")
						refine(population, numAttempts - 1, tolerance)
					case Some(newPop) =>
						//Next tolerance is the median of the previous best for each particle
						val medianTolerance = newPop.map(_.bestFit).toEmpiricalSeq.quantile(Probability(0.5))
						refine(newPop, numAttempts - 1, medianTolerance)
				}
			}
		}
		
		val result = refine(uniformlyWeightedParticles, model.abcParameters.refinements, model.abcParameters.tolerance)
		result.map(_.value)
	}
}

case class ABCParameters(
		reps: Int, 
		numParticles: Int, 
		tolerance: Double, 
		refinements: Int, 
		particleRetries: Int = 100, 
		particleChunking: Int = 100
)
