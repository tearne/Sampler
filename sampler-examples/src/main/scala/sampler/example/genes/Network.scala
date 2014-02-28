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

case class Infection(fid: Int, currentVirus: Virus, originalVirus: Virus, source: Option[Int])
	
object Infection {
	def possiblyMutate(current: Set[Infection]): Set[Infection] = {
		current.map{infection => infection.copy(currentVirus = infection.currentVirus.possiblyMutate)}
	}
	def toSeq(infection: Infection): Seq[Any] = {
		import infection._
		fid +: originalVirus.sequence ++: Seq(source.getOrElse("-"))
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
	val mutationRate = 1.0
	val sequenceLength = 50
	val randomBase = Distribution(r.nextInt(4))
	val fresh = Virus(IndexedSeq.fill(sequenceLength)(0))
	val mutates = Distribution.bernouliTrial(mutationRate)	
	val randomIdx = Distribution.uniform(0, sequenceLength)
}

case class DifferenceMatrix(matrix: Map[(Int, Int), Int]){
	def getOrElse(fidA: Int, fidB: Int, fallback: Int): Int = {
		if(fidA < fidB) {
			matrix.getOrElse((fidB, fidA),fallback)
		}
		else {
			matrix.getOrElse((fidA, fidB),fallback)
		} 
	}
	
	lazy val infectedFarms = matrix.keySet.foldLeft(Set.empty[Int]){case (acc, pair) => acc + pair._1 + pair._2}
	
	def toRows: Seq[Seq[Int]] = matrix.map{case ((a,b), diff) =>
		Seq(a,b,diff)
	}.toSeq
}

object DifferenceMatrix{
	def apply(entries: Traversable[Traversable[String]]): DifferenceMatrix = {
		val map = entries.map{row =>
			val indexed = row.toIndexedSeq
			assert(row.size == 3)
			((indexed(0).toInt ,indexed(1).toInt), indexed(2).toInt)
		}.toMap
		
		DifferenceMatrix(map)
	}
	def apply(outbreak: Outbreak): DifferenceMatrix = {
		val virusMap: Map[Int, Virus] = outbreak.infections.map{i => i.fid -> i.originalVirus}.toMap
		
		val pairs = for{
			a <- 0 to Parameters.farmIdRange.max
			b <- 0 until a
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
			val diff =  getDifference(pair._1, pair._2)
			(pair, diff)
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
		val result = Outbreak(infections ++ newInfections.filter{case Infection(fid, _, _, _) => !infected.contains(fid)})
		result
	}
	
	lazy val differenceMatrix = DifferenceMatrix(this)
}
object Outbreak{
	def seed(fid: Int) = {
		val freshVirus = Virus.fresh
		Outbreak(Set(Infection(fid, freshVirus, freshVirus, None)))
	}
	def updateCurrentMutations(current: Outbreak) = Outbreak{
		//TODO use lens
		current.infections.map{i => i.copy(currentVirus = i.currentVirus.possiblyMutate)}
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
	
	val gridSize = 5
	val farmIdRange = new {
		val min = 0
		val max = gridSize * gridSize - 1
		val size = max + 1
	}
	val allFarmIds = (farmIdRange.min to farmIdRange.max).toSet

	val localSpreadPrior = UniformPrior(0.0, 1.0)
	
	def position(fid: Int) = (fid % Parameters.gridSize, fid / Parameters.gridSize)
	
	def neighboursIncludingSelf(id: Int): Set[Int] = {
		assert(id <= farmIdRange.max && id >= 0,  s"Farm ID $id out of range")
		val (xPos, yPos) = position(id)
		val maxAxis = gridSize - 1
		def axisNbrs(idx: Int): Set[Int] = idx match {
			case 0 => Set(0, 1)
			case `maxAxis` => Set(maxAxis - 1, maxAxis)
			case _ => Set(idx - 1, idx, idx + 1)
		}
		val includingSelf = for{
			x <- axisNbrs(xPos)
			y <- axisNbrs(yPos)
		} yield(x + y * gridSize)
		includingSelf
	}
	
	def companyIncludingSelf(fid: Int) = {
		//note, this can't accomodate company sharing premises
		if(NetworkModel.companyA.contains(fid))
			NetworkModel.companyA
		else if(NetworkModel.companyB.contains(fid))
			NetworkModel.companyB
		else Set.empty[Int]
	}
	
	def spatialKernelSupport(fid: Int) = neighboursIncludingSelf(fid) ++ companyIncludingSelf(fid)
	
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
			localSpreadPrior.density(localTransmission) / Parameters.farmIdRange.max.toDouble
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
//	println("0: "+Parameters.neighboursIncludingSelf(0))
//	println("3: "+Parameters.neighboursIncludingSelf(3))
//	println("6: "+Parameters.neighboursIncludingSelf(6))
//	println("15: "+Parameters.neighboursIncludingSelf(15))
	
	val truth = Parameters(0.05, 5)
	val outbreak = NetworkModel.outbreakDistribution(truth).sample
	
	val diffMatrix = outbreak.differenceMatrix
	val infectedA = diffMatrix.infectedFarms
	val infectedB = outbreak.infected
	println(infectedA)
	println(infectedB)
	println(infectedA == infectedB)
	
	val wd = Paths.get("results").resolve("Network")
	val header = Seq("FarmA", "FarmB", "Diff")
	CSV.writeLines(wd.resolve("diffMatrix.csv"), header +: diffMatrix.toRows)
}
object NetworkModel {
	implicit val random = Random
	val companyA = Set(5,8,17)
	val companyB = Set(0,18,20)
	
	val runLengthDays = 7
	
	def outbreakDistribution(p: Parameters): Distribution[Outbreak] = {
		val localSpread = Distribution.bernouliTrial(p.localTransmission)
		val companySpread = Distribution.bernouliTrial(0.3)
		
		def addNewInfections(current: Outbreak): Outbreak = {
			if(current.infected.size == Parameters.farmIdRange.size) current
			else {
				val newInfections: Set[Infection] = current.infections.flatMap{from =>
					val localNeighbours = Parameters.neighboursIncludingSelf(from.fid) - from.fid
					val locallyInfected = localNeighbours.filter(_ => localSpread.sample)

					val companyNbrs = Parameters.companyIncludingSelf(from.fid) - from.fid
					val companyInfected = companyNbrs.filter(_ => companySpread.sample)

					val currentVirus = from.currentVirus
					
					val infections = (companyInfected ++ locallyInfected).map{to => 
						Infection(to, currentVirus, currentVirus, Some(from.fid))
					}
					
//					println(s"${from.fid} infected ${infections.map(_.fid)}")
					
					infections
				}
				
				current.add(newInfections)
			}
		}
		
		@tailrec
		def iterateDays(current: Outbreak, daysLeft: Int): Outbreak = {
			if(daysLeft == 0 || current.size == Parameters.farmIdRange.size) current
			else {
//				println("----")
				val updated = addNewInfections(Outbreak.updateCurrentMutations(current))
				iterateDays(updated, daysLeft - 1)
			}
		}
		
		Distribution{
			iterateDays(Outbreak.seed(p.sourceFarm), runLengthDays)
		}
	}
}
class NetworkModel(val obsDiffMatrix: DifferenceMatrix) extends Model[Parameters]{
	
	/*
  *  		0	1	2	3	4	5	6	7	8	9
  *    ---------------------------------------------
  * 	9|	.	.	.	.	.	.	.	.	.	.
  * 	8|	.	.	.	.	.	.	.	.	.	.
  * 	7|	.	.	.	.	.	.	.	.	A	.
  * 	6|	.	.	.	.	.	.	.	.	.	.
  * 	5|	.	.	.	.	.	.	.	.	.	.
  * 	4|	.	.	.	.	.	.	.	.	.	.
  * 	3|	.	.	.	.	.	.	.	.	.	.
  * 	2|	.	A	.	.	.	.	.	.	.	.
  * 	1|	.	.	.	.	.	.	.	.	.	.
  * 	0|	.	. 	.	.	.	.	.	.	.	.
  *  
  */
	
	/*	4x4
	 * 
	 * 		20-	21	22	23	24
	 * 		15	16	17*	18-	19
	 * 		10	11	12	13	14
	 * 		5*	6	7	8*	9
	 * 		0-	1	2	3	4
	 * 
	 */
	
	def perturb(p: Parameters) = Parameters.perturb(p)
	def perturbDensity(a: Parameters, b: Parameters) = Parameters.perturbDensity(a, b)
	val prior = Parameters.prior
		
	def nodeDiff(simulated: Outbreak) = 
		(simulated.infected.diff(obsDiffMatrix.infectedFarms) union obsDiffMatrix.infectedFarms.diff(simulated.infected)).size

	def sizeDiff(simulated: Outbreak) = 
		math.abs(simulated.size - obsDiffMatrix.infectedFarms.size)
		
	def sequenceBasedDiff(simulated: Outbreak) = {
		val fallback = 5
		val simPairs = simulated.infections.filter(_.source.isDefined).map(infection => (infection.source.get, infection.fid))
		simPairs.map{case pair => obsDiffMatrix.getOrElse(pair._1, pair._2, fallback)}.sum
	}

	def sequenceThreshold(simulated: Outbreak) = {
		val fallback = 10
		val simPairs = simulated.infections.filter(_.source.isDefined).map(infection => (infection.source.get, infection.fid))
		simPairs.map{case pair => 
			val matrixDiff = obsDiffMatrix.getOrElse(pair._1, pair._2, fallback)
			if(matrixDiff <= 3) 0
			else 10
		}.sum
	}
	
	def distanceToObservations(p: Parameters): Distribution[Double] = {
		NetworkModel.outbreakDistribution(p).map(outbreak => nodeDiff(outbreak) + sizeDiff(outbreak))
//		NetworkModel.outbreakDistribution(p).map(outbreak => nodeDiff(outbreak) + sizeDiff(outbreak) + sequenceThreshold(outbreak))
	}
}

object ABCApp extends App{
	val wd = Paths.get("results").resolve("Network")
	Files.createDirectories(wd)
	
	val obsPath = Paths.get("results").resolve("Network").resolve("diffMatrix.csv")
	val csvLines = CSV.read(obsPath).toIndexedSeq
	assert(csvLines(0) == Seq("FarmA", "FarmB", "Diff"))
	val obsDiffMatrix = DifferenceMatrix(csvLines.drop(1))
	
	//val observedDiffMatrix = NetworkModel.outbreakDistribution(truth).sample.differenceMatrix
	println("Outbreak = "+obsDiffMatrix.infectedFarms)
	println("Outbreak Size = "+obsDiffMatrix.infectedFarms.size)
	val model = new NetworkModel(obsDiffMatrix)

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
				xlim(0,5) + ylim(0,5) +
				geom_bin2d(binwidth  = c(0.999,0.999))
			
			dev.off()
		"""
		ScriptRunner.apply(rScript, wd.resolve("script.r"))
			
	}
		
	ABC(model, abcParams, abcReporting)
}