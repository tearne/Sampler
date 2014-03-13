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

case class Infection(currentVirus: Virus, originalVirus: Virus, source: Option[Int])
	
object Infection {
	def possiblyMutate(current: Set[Infection]): Set[Infection] = {
		current.map{infection => infection.copy(currentVirus = infection.currentVirus.possiblyMutate)}
	}
}

case class Virus(sequence: IndexedSeq[Int]){
	assert(sequence.size == Virus.sequenceLength)
	import Virus._
	
	def possiblyMutate() = {
		if(mutates.sample) {
			val mutatedSeq = sequence.updated(randomIdx.sample, randomBase.sample)
			Virus(mutatedSeq)
		}
		else this
	}
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
	def contains(fidA: Int, fidB: Int): Boolean = {
		if(fidA < fidB) matrix.contains((fidB, fidA))
		else matrix.contains((fidA, fidB))
	}
	def getOrElse(fidA: Int, fidB: Int, fallback: Int): Int = {
		if(fidA < fidB) {
			matrix.getOrElse((fidB, fidA),fallback)
		}
		else {
			matrix.getOrElse((fidA, fidB),fallback)
		} 
	}
	def apply(fidA: Int, fidB: Int): Int = {
		if(fidA < fidB) matrix((fidB, fidA))
		else matrix((fidA, fidB))
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
		val virusMap: Map[Int, Virus] = outbreak.infections.map{case (fid, infection) => fid -> infection.originalVirus}.toMap
		
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


final case class Outbreak(infections: Map[Int, Infection]){
	lazy val infected = infections.keySet
	lazy val size = infections.size
	
	def add(newInfections: Map[Int, Infection]): Outbreak = {
		val newInfectionsMap = newInfections.foldLeft(infections){case (acc, (newFid, newInf)) => 
			if(infections.contains(newFid)) acc
			else acc + (newFid -> newInf)
		}
		
		Outbreak(newInfectionsMap)
	}
	
	def thinObservations(proportion: Double, ensureFid: Int): Outbreak = {
		val selected = Distribution.bernouliTrial(proportion)(Random)
		Outbreak(infections.filter{case (fid, _) => selected.sample || fid == ensureFid})
	}
	
	lazy val differenceMatrix = DifferenceMatrix(this)
}
object Outbreak{
	def seed(fid: Int) = {
		val freshVirus = Virus.fresh
		Outbreak(Map(fid -> Infection(freshVirus, freshVirus, None)))
	}
	def updateCurrentMutations(current: Outbreak) = Outbreak{
		current.infections.mapValues{i => i.copy(currentVirus = i.currentVirus.possiblyMutate)}
	}
}

case class UniformPrior(min: Double, max: Double){
	def density(x: Double) = if(x <= max && x >= min) 1.0/(max-min) else 0
	def sample(r: Random) = r.nextDouble(min, max)
}

case class Parameters(localTransmission: Double){
	def toSeq: Seq[Any] = {
		Seq(localTransmission)
	}
}
object Parameters {
	implicit val r = Random
	
	val names = Seq("LocalTransmission")
	
	val gridSize = 10
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
		Parameters(perturbUnit(localTransmission))
	}
	
	val prior = new Prior[Parameters]{
		def density(p: Parameters) = {
			import p._
			localSpreadPrior.density(localTransmission)
		}
		def sample = Parameters(localSpreadPrior.sample(r))
	}
	
	def perturbDensity(a: Parameters, b: Parameters): Double =
		kernel.density(a.localTransmission - b.localTransmission)
}

object Generate extends App{
	val truth = Parameters(0.3)
	val fullOutbreak = NetworkModel.outbreakDistribution(truth).sample
	
	val observedOutbreak = fullOutbreak//.thinObservations(0.8, NetworkModel.seedFarm)
	
	println("Full: "+fullOutbreak.size+"  -  "+fullOutbreak.infected)
	println(" Obs: "+observedOutbreak.size+"  -  "+observedOutbreak.infected)
	
	val wd = Paths.get("results").resolve("Network")
	val header = Seq("FarmA", "FarmB", "Diff")
	CSV.writeLines(wd.resolve("diffMatrix.csv"), header +: observedOutbreak.differenceMatrix.toRows)
}
object NetworkModel {
	implicit val random = Random
	val companyA = Set.empty[Int]//Set(0, 5, 11, 23, 25, 27, 46, 48, 52, 77, 86, 92)
	val companyB = Set(13, 42, 44, 61, 63, 84)
	
	val seedFarm = 4
	val runLengthDays = 7
	
	def outbreakDistribution(p: Parameters): Distribution[Outbreak] = {
		val localSpread = Distribution.bernouliTrial(p.localTransmission * 0.4)
		val companySpread = Distribution.bernouliTrial(0.2)
		
		def addNewInfections(current: Outbreak): Outbreak = {
			if(current.infected.size == Parameters.farmIdRange.size) current
			else {
				val infectionMap = current.infections.foldLeft(current.infections){case (acc, (iFid, infection)) => 
			 		val newVictims = {
				 		val localNeighbours = Parameters.neighboursIncludingSelf(iFid) - iFid
		 	 			val locallyInfected = localNeighbours.filter(_ => localSpread.sample)
		 	 			val companyNbrs = Parameters.companyIncludingSelf(iFid) - iFid
		 	 			val companyInfected = companyNbrs.filter(_ => companySpread.sample)
	
		 	 			(companyInfected ++ locallyInfected).filter(newInfection => !acc.contains(newInfection))	//Don't allow re-infection
				 	}
			    
				 	acc.++(newVictims.map(fid =>
				 		(fid, Infection(infection.currentVirus, infection.currentVirus, Some(iFid)))
				 	))
				}
				
				Outbreak(infectionMap)
			}
		}
		
		@tailrec
		def iterateDays(current: Outbreak, daysLeft: Int): Outbreak = {
			if(daysLeft == 0 || current.size == Parameters.farmIdRange.size) current
			else {
				val updated = addNewInfections(Outbreak.updateCurrentMutations(current))
				iterateDays(updated, daysLeft - 1)
			}
		}
		
		Distribution{
			iterateDays(Outbreak.seed(seedFarm), runLengthDays)
		}
	}
}
class NetworkModel(val obsDiffMatrix: DifferenceMatrix) extends Model[Parameters]{
	
	/*	10x10 
	 * 
	 * 		90	91	92+	93	94	95	96	97	98	99
	 *   	80	81=	82	83	84=	85	86+	87	88	89
	 *  	70	71	72	73	74	75	76	77+	78	79
	 *  	60	61=	62	63=	64	65	66	67	68	69
	 *  	50	51	52+	53	54	55	56	57	58	59
	 *  	40	41	42=	43	44=	45	46+	47	48+	49
	 *  	30	31	32	33	34	35	36	37	38	39
	 * 		20	21	22	23+	24	25+	26	27+	28	29
	 * 		10	11+	12	13=	14	15	16	17	18	19
	 * 		_0+	_1	_2	_3	_4	_5+	_6	_7	_8	_9
	 * 
	 * 
	 * 		+:	0, 5, 11, 23, 25, 27, 46, 48, 52, 77, 86, 92
	 *   	=:	13, 42, 44, 61, 63, 84
	 * 
	 * 
	 * 
	 * 
	 * 	5x5
	 * 
	 * 		20#	21	22	23	24
	 * 		15	16	17*	18#	19
	 * 		10	11	12	13	14
	 * 		5*	6	7	8*	9
	 * 		0	1#	2	3	4
	 * 
	 */
	
	def perturb(p: Parameters) = Parameters.perturb(p)
	def perturbDensity(a: Parameters, b: Parameters) = Parameters.perturbDensity(a, b)
	val prior = Parameters.prior
		
	def simCoverObs(simulated: Outbreak) = {
		simulated.infected.diff(obsDiffMatrix.infectedFarms).size
	}
	
	def obsCoverSim(simulated: Outbreak) = {
		obsDiffMatrix.infectedFarms.diff(simulated.infected).size
	}

	def sizeDiff(simulated: Outbreak) = { 
		math.abs(simulated.size - obsDiffMatrix.infectedFarms.size)
	}
		
	def matrixSimalirity(simulated: Outbreak): Double = {
		val simDiffMatrix = simulated.differenceMatrix
		
		def matrixDistance(matrixA: DifferenceMatrix, matrixB: DifferenceMatrix, penalty: Double): Double = {
			matrixA.matrix.foldLeft(0.0){case (acc, ((farmA, farmB), aDiff)) =>
				if(!matrixB.contains(farmA, farmB)){
					acc + penalty
				}
				else{
					val bDiff = matrixB(farmA, farmB)
					val diff = math.abs(bDiff - aDiff)
					acc + {
						if(diff > 9) 1
						else 0
					}
					
				}
			}			
		}
		
//		matrixDistance(simDiffMatrix, obsDiffMatrix, 0)		// Pull to left
		matrixDistance(obsDiffMatrix, simDiffMatrix, 0)		// Pull to right if using penalty, else left
	}
	
	def distanceToObservations(p: Parameters): Distribution[Double] = {
//		NetworkModel.outbreakDistribution(p).map(outbreak => sizeDiff(outbreak) + numObservedMissing(outbreak))
//		println("p = "+p)
//		println("Observed = "+obsDiffMatrix.infectedFarms)
//		NetworkModel.outbreakDistribution(p).map(outbreak => numObservedMissing(outbreak))
		
		
		// 1, 40
		// 2, 70
		// 6, 200
		// 10 , 400
//		NetworkModel.outbreakDistribution(p).map(outbreak => matrixSimalirity(outbreak) + 10 * numObservedMissing(outbreak))
		
		//Try saturating with more company farms to see if seq can refine
		
		NetworkModel.outbreakDistribution(p).map(outbreak => matrixSimalirity(outbreak) + 10*(simCoverObs(outbreak) + obsCoverSim(outbreak)))
//		NetworkModel.outbreakDistribution(p).map(outbreak => 10*(simCoverObs(outbreak) + obsCoverSim(outbreak)))
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
	println("Observations = "+obsDiffMatrix.infectedFarms)
	println("        Size = "+obsDiffMatrix.infectedFarms.size)
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
			statsTab = c(
				quantile(posterior$$LocalTransmission, c(0.025, 0.5, 0.975)),
				mean = mean(posterior$$LocalTransmission)
			)
			statsDF = data.frame(variable = names(statsTab), value = as.vector(statsTab))
			
			pdf("density.$generationId%02d.pdf", width=4.13, height=2.91) #A7 landscape paper
			ggplot(melt(posterior), aes(x = value)) + 
				geom_density() +
				geom_vline(data = statsDF, aes(xintercept = value, colour = variable), show_guide = TRUE) +
				scale_x_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1))
			dev.off()
		"""
		ScriptRunner.apply(rScript, wd.resolve("script.r"))
			
	}
		
	ABC(model, abcParams, abcReporting)
}