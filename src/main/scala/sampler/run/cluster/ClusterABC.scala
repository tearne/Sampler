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
	
	def initModel(p: Parameters, obs: Observations): Samplable[Output,R]
	val prior: Prior[Parameters, R]
	def samplePrior() = prior.sample(random)
	
	val observations: Observations
	val abcParameters: ABCParameters
	val builder: SampleBuilder
	val random: R
	
	def nextParticle(
			population: Seq[Particle[Parameters]],
			tolerance: Double
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

trait EncapsulatedEnvironment[R <: Random] extends Serializable{
	type E <: ABCModel[R]
	val env: E
	val population: Seq[Particle[env.Parameters]]
}

object Encapsulator{
	def apply[R <: Random](abc0: ABCModel[R])(population0: Seq[Particle[abc0.Parameters]]) = new EncapsulatedEnvironment[R] with Serializable{
		type E = abc0.type
		val env: E = abc0
		val population = population0
	}
}

object ABCBase{
	def apply[R <: Random](
			model: ABCModel[R],
			runner: ClusterRunner
	): Seq[model.Parameters] = {
		type P = model.Parameters
		
		val uniformlyWeightedParticles = (1 to model.abcParameters.numParticles).par.map(i => Particle(model.samplePrior, 1.0, Double.MaxValue)).seq
		
		def evolve(population: Seq[Particle[P]], tolerance: Double): Option[Seq[Particle[P]]] = {
			import model._
			println("Now working on tolerance = "+tolerance)
			
			//Map the sequence of weighted params (particles) to a map from param to (summed) weight 
			val samplable = population.groupBy(_.value).map{case (k,v) => (k,v.map(_.weight).sum)}.toEmpiricalWeighted
			
			//How many particles generated per job?
			val jobSizes = (1 to abcParameters.numParticles)
				.grouped(abcParameters.particleChunking)
				.map(_.size).toSeq
			println(s"JobSizes: $jobSizes")
			
			//TODO JobRunner Abortable Job syntax too noisy
			val encap = Encapsulator.apply(model)(population)
			
			//Check serializable
			val baos = new ByteArrayOutputStream()
			val oos = new ObjectOutputStream(baos)
			try{
				val tmp = oos.writeObject(encap)
				println("object "+encap)
				println("Object stream = "+tmp)
			}catch{
				case e: Throwable => throw new RuntimeException("Bleh!", e)
			}
			
			val jobs = jobSizes.map(numParticles => {
				println("======================================")
				ABCJob(numParticles, tolerance, encap)
			}).toList
			val runnerResults: Seq[Option[EncapsulatedEnvironment[R]]] = runner(jobs)
			//val t1 = runnerResults.map{_.poulation}	
			implicit def asTrav(seq: Seq[Particle[P]]) = seq.toTraversable
			
			val newParticles = runnerResults.view
				.takeWhile(_.isDefined)
				.map(_.get.population.asInstanceOf[Seq[Particle[P]]])
				.force
				.flatten
			
			if(newParticles.size == abcParameters.numParticles) Some(newParticles)
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
