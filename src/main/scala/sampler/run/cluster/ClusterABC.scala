package sampler.run.cluster

import sampler.data.Samplable
import sampler.math.{Random, Probability}
import sampler.data.SampleBuilder
import sampler.data.Empirical._
import scala.annotation.tailrec
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream

trait Prior[A,Rnd] extends Samplable[A,Rnd]{
	def density(value: A): Double
}
case class Particle[A](value: A, weight: Double, bestFit: Double)

trait ABCModel[R <: Random]{
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
	
	def samplableModel(p: Parameters, obs: Observations): Samplable[Output,R]
	val prior: Prior[Parameters, R]
	def samplePrior() = prior.sample(random)
	
	val observations: Observations
	val meta: ABCMeta
	val builder: SampleBuilder
	val random: R
}

trait EncapsulatedPopulation[R <: Random] extends Serializable{ self =>
	type M <: ABCModel[R]
	val model: M
	val population: Seq[Particle[model.Parameters]]
	
//	def update(population0: Seq[Particle[model.Parameters]]) = new EncapsulatedPopulation[R] with Serializable{
//		type M = self.M
//		val model: M = self.model
//		val population: Seq[Particle[model.Parameters]] = population0
//	}
}

object Encapsulator{
	def apply[R <: Random](model0: ABCModel[R])(population0: Seq[Particle[model0.Parameters]]) = new EncapsulatedPopulation[R] with Serializable{
		type M = model0.type
		val model: M = model0
		val population = population0
	}
}

object ABCBase{
	import sampler.data.SerialSampleBuilder
	
	def init[R <: Random](model: ABCModel[R]): EncapsulatedPopulation[R] = {
		val numParticles = model.meta.numParticles
		val initialPopulation = (1 to numParticles).par.map(i => Particle(model.samplePrior, 1.0, Double.MaxValue)).seq
		Encapsulator(model)(initialPopulation)
	}
	
	def generateParticles[R <: Random](
			ePop: EncapsulatedPopulation[R], 
			quantity: Int, 
			tolerance: Double
	): Option[EncapsulatedPopulation[R]] = {
		type Params = ePop.model.Parameters
		import ePop.model._
		import ePop.population
		
		@tailrec
		def nextParticle(failures: Int = 0): Option[Particle[Params]] = {
			//if(!keepGoing.get) None
			//else  
			if(failures >= meta.particleRetries) None
			else{
				def getScores(params: Params) = {
					val modelWithMetric = samplableModel(params, observations).map(_.distanceTo(observations))
					val modelWithScores = SerialSampleBuilder(modelWithMetric)(_.size == meta.reps)(random)
						.filter(_ <= tolerance)
					modelWithScores
				}
				
				def getWeight(params: Parameters, numPassed: Int) = {
					val fHat = numPassed.toDouble / meta.reps
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
					case None => nextParticle(failures + 1)
				}
			}
		}
		
		val particles = (1 to quantity)
			.view
			.map(i => nextParticle())
			.takeWhile(_.isDefined)
			.map(_.get)
			.force
		if(particles.size == quantity) Some(Encapsulator(ePop.model)(particles))
		else None
	}
	
	def evolveOnce[R <: Random](
			ePop: EncapsulatedPopulation[R], 
			runner: ClusterRunner,
			tolerance: Double
	): Option[EncapsulatedPopulation[R]] = {
		type Params = ePop.model.Parameters
		import ePop.model
		import model.meta
		
		println("Now working on tolerance = "+tolerance)
		
		//How many particles to be generated per job?
		val jobSizes = (1 to meta.numParticles)
			.grouped(meta.particleChunking)
			.map(_.size).toSeq
		println(s"JobSizes: $jobSizes")
		
		//Check serializable
//		val baos = new ByteArrayOutputStream()
//		val oos = new ObjectOutputStream(baos)
//		try{
//			val tmp = oos.writeObject(encap)
//			println("object "+encap)
//			println("Object stream = "+tmp)
//		}catch{
//			case e: Throwable => throw new RuntimeException("Bleh!", e)
//		}
			
		val jobs = jobSizes.map(numParticles => Job{() =>
			ABCBase.generateParticles(ePop, numParticles, tolerance)
		}).toList
		val runnerResults: Seq[Option[EncapsulatedPopulation[R]]] = runner(jobs)
			
		val newParticles = runnerResults.view
			.takeWhile(_.isDefined)
			.map(_.get.population.asInstanceOf[Seq[Particle[Params]]])
			.force
			.flatten
			
		if(newParticles.size == meta.numParticles) Some(Encapsulator(model)(newParticles))
		else None
	}
		
	def evolve[R <: Random](
			ePop: EncapsulatedPopulation[R], 
			runner: ClusterRunner
	): Option[EncapsulatedPopulation[R]] = {
		type Params = ePop.model.Parameters
		import ePop.model
		import model.meta
		
		@tailrec
		def refine(
				ePop: EncapsulatedPopulation[R], 
				numAttempts: Int, 
				tolerance: Double
		): Option[EncapsulatedPopulation[R]] = {
			println("Generations left to go "+numAttempts)
			if(numAttempts == 0) Some(ePop)
			else{
				evolveOnce(ePop, runner, tolerance) match {
					case None =>
						println("Failed to refine current population, evolving within same tolerance")
						refine(ePop, numAttempts - 1, tolerance)
					case Some(newEPop) =>
						//Next tolerance is the median of the previous best for each particle
						val medianTolerance = newEPop.population.map(_.bestFit).toEmpiricalSeq.quantile(Probability(0.5))
						refine(newEPop, numAttempts - 1, medianTolerance)
				}
			}
		}
		
		refine(ePop, meta.refinements, meta.tolerance)
	}
}

case class ABCMeta(
		reps: Int, 
		numParticles: Int, 
		tolerance: Double, 
		refinements: Int, 
		particleRetries: Int = 100, 
		particleChunking: Int = 100
)
