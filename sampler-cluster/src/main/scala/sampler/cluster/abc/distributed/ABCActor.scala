package sampler.cluster.abc.distributed

import akka.actor.Actor
import sampler.abc.ABCModel
import sampler.abc.Particle
import akka.actor.ActorLogging
import scala.language.existentials
import scala.concurrent.Future
import sampler.data.Distribution
import sampler.math.Partition
import sampler.math.Random
import scala.concurrent.duration._
import akka.actor.Props
import sampler.abc.ABCParameters
import sampler.cluster.abc.ABCJob
import sampler.math.Statistics
import sampler.Implicits._
import sampler.math.Probability
import akka.actor.ActorRef

case class Start()
case class Result(params: Seq[ABCModel#Parameters])

case class StatusRequest()
case class NewScoredParameters(pop: Seq[ScoredParam])
//TODO needs merge with ABCModel
case class ScoredParam(value: ABCModel#Parameters, scores: Seq[Double])
case class AugmentedParticle[A](value: A, weight: Double, scores: Seq[Double]){
	def toParticle = Particle(value, weight, scores.sum.toDouble / scores.size)
}

class ABCActor(val model: ABCModel, abcParams: ABCParameters, modelRunner: ModelRunner) extends Actor with ActorLogging{
	import model._
	import context._
	
	val broadcaster = context.actorOf(Props[Broadcaster], "broadcaster")
	val worker = context.actorOf(Props(new Worker(modelRunner)), "worker")
	context.system.scheduler.schedule(1.second, 1.second, self, Mix)
	
	case class Mix()

	implicit val r = Random
	var particleInBox = Set[AugmentedParticle[Parameters]]()
	var currentTolerance = Double.MaxValue
	var currentPopulation: Seq[AugmentedParticle[Parameters]] = Seq.empty
	var currentIteration = 0
	
	//Initialise
	var currentWeightsTable: IndexedSeq[(Parameters,Double)] = IndexedSeq.empty
	
	def receive = idle
	
	def idle: Receive = {
		case Start =>
			currentWeightsTable = {
				val zeroPopulation = (1 to abcParams.numParticles).par.map(i => 
					Particle(model.prior.sample(),1.0,Double.MaxValue)
				).seq
				worker ! ABCJob(zeroPopulation, Double.MaxValue, abcParams)
				log.info("Sent init job to {}", worker)
				zeroPopulation.map{particle => (particle.value, 1.0)}.toIndexedSeq
			}
			val sndr = sender
			become(busy(sndr))
	}
	
	def busy(requestor: ActorRef): Receive = {
		case NewScoredParameters(scoredParameters) => 
			particleInBox = particleInBox ++ scoredParameters
				.map{scoredParam =>
					convertToAugmentedParticle(scoredParam)
				}.collect{
					case Some(particle) => particle
				}
			log.info(
					"{} new particles arrived from {}, size is now {}", 
					scoredParameters.size, 
					sender, 
					particleInBox.size
			)
			if(particleInBox.size >= abcParams.numParticles){
				if(currentIteration == abcParams.refinements){
					requestor ! particleInBox.map{p =>
						p.value
					}.toSeq
					worker ! Abort
				} else {
					val newPopulation = particleInBox.toSeq
					val newTolerance = calculateNewTolerance(newPopulation)
					log.info("New tolerance = "+newTolerance)
					//TODO this doesn't actually need tolerance
					log.info("Sending new ABC job")
					worker ! ABCJob(newPopulation.map{_.toParticle}, newTolerance, abcParams)
					particleInBox = Set.empty
					currentWeightsTable = newPopulation.groupBy(_.value).map{case (k,v) => (k, v.map(_.weight).sum)}.toIndexedSeq
					currentPopulation = newPopulation
					currentTolerance = newTolerance
					currentIteration = currentIteration + 1
				}
			}
		case sr: StatusRequest =>
			worker.forward(sr)
		case Mix =>
			if(particleInBox.size > 0){
				val tenAugmentedParticles = Distribution.uniform(particleInBox.toIndexedSeq).until(_.size == 10).sample
				val tenScoredParams = tenAugmentedParticles.map{particle => 
					ScoredParam(particle.value, particle.scores)
				}
				broadcaster ! RandomSend(NewScoredParameters(tenScoredParams))
			}
		case msg => 
			throw new UnsupportedOperationException("Unexpected message "+msg)
	}
	
	def convertToAugmentedParticle(scoredParam: ScoredParam): Option[AugmentedParticle[Parameters]] = {
		val params = scoredParam.value.asInstanceOf[model.Parameters]

		def getWeight(params: Parameters, scores: Seq[Double]): Option[Double] = {
			val fHat = scores.filter(_ < currentTolerance).size.toDouble
			val numerator = fHat * prior.density(params)
			val denominator = currentWeightsTable.map{case (params0, weight) => 
				weight * params0.perturbDensity(params)
			}.sum
			if(numerator > 0 && denominator > 0) Some(numerator / denominator)
			else None
		}
		
		val res: Option[AugmentedParticle[Parameters]] = for{
			weight <- getWeight(params, scoredParam.scores) 
		} yield AugmentedParticle(params, weight, scoredParam.scores)
		
		res
	}
	
	def calculateNewTolerance(particles: Seq[AugmentedParticle[Parameters]]): Double = {
		val medianMeanFit = Statistics.quantile(particles.map{_.toParticle.meanFit}.toIndexedSeq.toEmpiricalSeq, Probability(0.5))
		if(medianMeanFit == 0) {
			log.warning("Median of mean scores from last generation evaluated to 0, half the previous tolerance will be used instead.")
			currentTolerance / 2
		}
		else math.min(medianMeanFit, currentTolerance)
	}
}