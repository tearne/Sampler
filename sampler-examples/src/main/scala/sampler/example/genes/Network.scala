package sampler.example.genes

import java.nio.file.Files
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.language.reflectiveCalls
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import com.typesafe.config.ConfigFactory
import sampler.cluster.abc.ABC
import sampler.cluster.abc.Model
import sampler.cluster.abc.Prior
import sampler.cluster.abc.actor.Report
import sampler.cluster.abc.config.ABCConfig
import sampler.data.Distribution
import sampler.io.CSV
import sampler.math.Random
import sampler.r.ScriptRunner
import scala.collection.immutable.BitSet

case class Infection(fid: Int, virus: Virus, source: Option[Int])
	
object Infection {
	def possiblyMutate(current: Set[Infection]): Set[Infection] = {
		current.map{infection => infection.copy(virus = infection.virus.possiblyMutate)}
	}
}

case class Virus(sequence: IndexedSeq[Int]){
	assert(sequence.size == Virus.sequenceLength)
	import Virus._
	
	def possiblyMutate() = {
		if(mutates.sample) {
//			println("From :"+sequence)
			val mutatedSeq = sequence.updated(randomIdx.sample, randomBase.sample)
//			println("To   :"+mutatedSeq)
			Virus(mutatedSeq)
		}
		else this
	}
	
//	override def toString = s"Genes[~${sequence.size}]"
}

object Virus{
	implicit val r = Random	//TODO randoms all over the place
	val mutationRate = 0.5
	val sequenceLength = 10
	val randomBase = Distribution(r.nextInt(4))
	val fresh = Virus(IndexedSeq.fill(sequenceLength)(0))
	val mutates = Distribution.bernouliTrial(mutationRate)	
	val randomIdx = Distribution.uniform(0, sequenceLength)
}

case class DifferenceMatrix(map: Map[(Int, Int), Int]){
	def apply(fidA: Int, fidB: Int): Int = {
		if(fidA < fidB) {
//			println((fidB, fidA))
			map.getOrElse((fidB, fidA),0)
		}
		else {
//			println((fidA, fidB))
			map.getOrElse((fidA, fidB),0)
		} 
	}
}

object DifferenceMatrix{
	def apply(outbreak: Outbreak): DifferenceMatrix = {
		val virusMap: Map[Int, Virus] = outbreak.infections.map{i => i.fid -> i.virus}.toMap
		
		val pairs = for{
			a <- 0 to 99
			b <- 0 to a
		} yield (a,b)
		
		def getDifference(aFid: Int, bFid: Int): Option[Int] = {
			for{
				vA <- virusMap.get(aFid)
				vB <- virusMap.get(bFid)
			} yield {
				vA.sequence.zip(vB.sequence).foldLeft(0){case (acc, (a,b)) => acc + (if(a == b) 0 else 1)}
			}
		}
		
		val map = pairs.view.map{pair => 
			(pair, getDifference(pair._1, pair._2))
		}.collect{
			case (pair, Some(diff)) => (pair, diff)
		}.toMap
			
		DifferenceMatrix(map)
	}
}


case class Outbreak(infections: Set[Infection]){
	lazy val infected = infections.map(_.fid)
	lazy val size = infections.size
	
	def add(newInfections: Set[Infection]) = {
		//Don't allow re-infection with new strain
		Outbreak(infections ++ newInfections.filter{case Infection(fid, virus, _) => !infected.contains(fid)})
	}
	
	lazy val differenceMatrix = DifferenceMatrix(this)
}
object Outbreak{
	def seed(fid: Int) = Outbreak(Set(Infection(fid, Virus.fresh, None)))
	def updateMutations(current: Outbreak) = Outbreak{
		//TODO use lens
		current.infections.map{i => i.copy(virus = i.virus.possiblyMutate)}
	}
}

case class UniformPrior(min: Double, max: Double){
	def density(x: Double) = if(x <= max && x >= min) 1.0/(max-min) else 0
	def sample(r: Random) = r.nextDouble(min, max)
}

case class Parameters(localTransmission: Double, sourceFarm: Int){
	def toSeq: Seq[Any] = {
		val sourcePos = Parameters.position(sourceFarm)
		Seq(localTransmission, sourcePos._1, sourcePos._2)
	}
}
object Parameters {
	implicit val r = Random
	
	val names = Seq("LocalTransmission", "SourceX", "SourceY")
	
	val farmIdRange = new {
		val min = 0
		val max = 99
	}
	val allFarmIds = (farmIdRange.min to farmIdRange.max).toSet

	val localSpreadPrior = UniformPrior(0.0, 1.0)
	
	def position(fid: Int) = (fid % 10, fid / 10)
	
	def neighboursIncludingSelf(id: Int): Set[Int] = {
		assert(id < 100 && id >= 0,  "Farm ID out of range")
		val (xPos, yPos) = position(id)
		def axisNbrs(idx: Int): Set[Int] = idx match {
			case 0 => Set(0, 1)
			case 9 => Set(8, 9)
			case _ => Set(idx - 1, idx, idx + 1)
		}
		val includingSelf = for{
			x <- axisNbrs(xPos)
			y <- axisNbrs(yPos)
		} yield(x + y * 10)
		includingSelf
	}
	
	def companyNeigbours(fid: Int) = {
		if(NetworkModel.company.contains(fid))
			NetworkModel.company
		else Set.empty[Int]
	}
	
	def spatialKernelSupport(fid: Int) = neighboursIncludingSelf(fid) ++ companyNeigbours(fid)
	
	private val kernel = new Prior[Double] with Distribution[Double]{
		val normal = {
			val syncRand: RandomGenerator = new SynchronizedRandomGenerator(new MersenneTwister())
			new NormalDistribution(syncRand, 0, 0.1, NormalDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
		}
		def sample = normal.sample
		def density(at: Double) = normal.density(at)
	}
	
	
	def perturb(p: Parameters): Parameters = {
		def perturbUnit(u: Double): Double = {
				kernel.map(_ + u).filter(value => value <= 1.0 && value >= 0.0).sample
		}
		
		import p._
		Parameters(
			perturbUnit(localTransmission),
			Distribution.uniform(spatialKernelSupport(sourceFarm).toIndexedSeq).sample
		)
	}
	
	val prior = new Prior[Parameters]{
		def density(p: Parameters) = {
			import p._
			localSpreadPrior.density(localTransmission) / 100.0
		}
		def sample = Parameters(
			localSpreadPrior.sample(r), 
			Distribution.uniform(allFarmIds.toIndexedSeq).sample
		)
	}
	
	def perturbDensity(a: Parameters, b: Parameters): Double = {
		val aNbrs = spatialKernelSupport(a.sourceFarm)
		
		val spatialDensity = if(aNbrs.contains(b.sourceFarm)) 1.0 / aNbrs.size else 0
		val transmissionDensity = kernel.density(a.localTransmission - b.localTransmission)
		
		transmissionDensity * spatialDensity
	}
}

object Generate extends App{
	val source = 42
	val truth = Parameters(0.1, source)
	val outbreak = NetworkModel.outbreakDistribution(truth).sample
	
	
//	val diffMatrix = outbreak.differenceMatrix
//	diffMatrix.
}
object NetworkModel {
	implicit val random = Random
	val company = Set(8, 41, 99)
	
	val runLengthDays = 14
	
	def outbreakDistribution(p: Parameters): Distribution[Outbreak] = {
		val localSpread = Distribution.bernouliTrial(p.localTransmission)
		val companySpread = Distribution.bernouliTrial(0.2)
		
		def addNewInfections(current: Outbreak): Outbreak = {
			if(current.infected.size == 100) current
			else {
				val newInfections: Set[Infection] = current.infections.flatMap{from =>
					val localNeighbours = Parameters.neighboursIncludingSelf(from.fid) - from.fid
					val locallyInfected = localNeighbours.filter(_ => localSpread.sample)

					val companyInfected = Parameters.companyNeigbours(from.fid).filter(_ => companySpread.sample)
					
					(companyInfected ++ locallyInfected).map(to => Infection(to, from.virus, Some(from.fid)))
				}
				
				current.add(newInfections)
			}
		}
		
		@tailrec
		def iterateDays(current: Outbreak, daysLeft: Int): Outbreak = {
			if(daysLeft == 0 || current.size >= 100) current
			else {
				val updated = addNewInfections(Outbreak.updateMutations(current))
				iterateDays(updated, daysLeft - 1)
			}
		}
		
		Distribution{
			iterateDays(Outbreak.seed(p.sourceFarm), runLengthDays)
		}
	} 
}
class NetworkModel(val observed: Outbreak) extends Model[Parameters]{
  /*
  *  		0	1	2	3	4	5	6	7	8	9
  *    ---------------------------------------------
  * 	9|	.	.	.	.	.	.	.	.	.	C
  * 	8|	.	.	.	.	.	.	.	.	.	.
  * 	7|	.	.	.	.	.	.	.	.	.	.
  * 	6|	.	.	.	.	.	.	.	.	.	.
  * 	5|	.	.	.	.	.	.	.	.	.	.
  * 	4|	.	C	.	.	.	.	.	.	.	.
  * 	3|	.	.	.	.	.	.	.	.	.	.
  * 	2|	.	.	.	.	.	.	.	.	.	.
  * 	1|	.	.	.	.	.	.	.	.	.	.
  * 	0|	.	. 	.	.	.	.	.	.	C	.
  *  
  */	
	def perturb(p: Parameters) = Parameters.perturb(p)
	def perturbDensity(a: Parameters, b: Parameters) = Parameters.perturbDensity(a, b)
	val prior = Parameters.prior
		
	def nodeDiff(simulated: Outbreak) = 
		(simulated.infected.diff(observed.infected) union observed.infected.diff(simulated.infected)).size

	def sizeDiff(simulated: Outbreak) = 
		math.abs(simulated.size - observed.size)
		
	def sequenceBasedDiff(simulated: Outbreak) = {
		val obsDiffMatrix = observed.differenceMatrix
		val simPairs = simulated.infections.filter(_.source.isDefined).map(infection => (infection.source.get, infection.fid))
		simPairs.map{case pair => obsDiffMatrix(pair._1, pair._2)}.sum
	}
		
	def distanceToObservations(p: Parameters): Distribution[Double] = {
		NetworkModel.outbreakDistribution(p).map(outbreak => 10 * (nodeDiff(outbreak) + sizeDiff(outbreak)) + sequenceBasedDiff(outbreak))
	}
}

object ABCApp extends App{
	val wd = Paths.get("results").resolve("Network")
	Files.createDirectories(wd)
	
	val truth = Parameters(0.1, 42)
	val observedData = NetworkModel.outbreakDistribution(truth).sample
	println("Siza = "+observedData.size)
	val model = new NetworkModel(observedData)

	val abcParams = ABCConfig.fromConfig(
		ConfigFactory.load,
		"network-example"
	)
	
	val abcReporting = { report: Report[Parameters] =>
		import report._
		
		val lines = Parameters.names +: posterior.map(_.toSeq)
		val fileName = f"posterior.$generationId%02d.csv"
		
		CSV.writeLines(
			wd.resolve(fileName),
			lines
		)
		
		val rScript = f"""
			lapply(c("ggplot2", "reshape", "hexbin"), require, character.only=T)
			
			posterior = read.csv("$fileName")
			
			pdf("density.$generationId%02d.pdf", width=4.13, height=2.91) #A7 landscape paper
			ggplot(posterior, aes(x = LocalTransmission)) + 
				geom_density() +
				scale_x_continuous(limits = c(0,0.5))

			ggplot(posterior, aes(SourceX, SourceY)) +
				xlim(0,10) + ylim(0,10) +
				geom_bin2d(binwidth  = c(0.999,0.999))
			
			dev.off()
		"""
		ScriptRunner.apply(rScript, wd.resolve("script.r"))
			
	}
		
	ABC(model, abcParams, abcReporting)
}