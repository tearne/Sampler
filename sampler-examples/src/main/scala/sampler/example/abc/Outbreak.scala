package sampler.example.abc

import java.nio.file.Files
import java.nio.file.Paths
import scala.annotation.tailrec
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
import sampler.data.ToEmpirical
import sampler.data.ToSamplable
import sampler.io.CSV
import sampler.math.Random
import sampler.math.Statistics
import sampler.r.ScriptRunner
import sampler.io.Logging

/*
 * This file contains a number of applications
 * 
 * 1. Generator
 * This is the first thing to run.  It generates an outbreak to use as
 * observed data and saves the resulting sequence difference matrix in a 
 * CSV file.
 * 
 * 2. ABCApplication
 * This reads the observed data (or a subset of it) and attempts to infer 
 * the transmission parameters which generated the outbreak.
 * 
 * 3. MetricTest
 * Running ABCApplication takes a long time, so a quicker way to examine
 * new metrics is to use this app.  It plots distributions of metric 
 * scores, so you can verify that the minimum values occur for the true
 * parameters which generated the outbreak. 
 */

object Truth{
	val parameters = Parameters(SpreadRates(0.2, 0.8))
//	val parameters = Parameters(SpreadRates(0.6, 0.2))
	val seedFarm = 4
}

object Generator extends App with Logging {
	val fullOutbreak = OutbreakModel.generate(
			Truth.parameters,
			Truth.seedFarm,
			SizeStopCondition(20),
			true
	)	
	log.info("Generated outbreak size: "+fullOutbreak.size)
	val companyInfections = fullOutbreak.infectionMap.collect{case (id, Infection(_,_,Some(Source(_,CompanyTransmission)))) => id}
	println(s"${companyInfections.size} company Infections: $companyInfections")
	
	val wd = Paths.get("results").resolve("Network")
	Files.createDirectories(wd)
	val header = Seq("FarmA", "FarmB", "Diff")
	CSV.writeLines(wd.resolve("diffMatrix.csv"), header +: fullOutbreak.differenceMatrix.toRows)
}

object ABCApplication extends App {
	val model = new ABCLinksModel(Data.loadProportionOfTrueOutbreakData(0.8))

	val params = ABCConfig.fromTypesafeConfig(
		ConfigFactory.load,
		"network-example"
	)
	
	val reporting = { report: Report[Parameters] =>
		import report._
		
		val lines = Parameters.names +: posterior.map(_.toSeq)
		val fileName = f"posterior.$generationId%03d.csv"
		
		CSV.writeLines(
			Data.wd.resolve(fileName),
			lines
		)
		
		val rScript = f"""
			lapply(c("ggplot2", "reshape"), require, character.only=T)
			
			posterior = read.csv("$fileName")

			stats = function(values, name){
                table = c(
                    quantile(values, c(0.05, 0.5, 0.95)),
                    mean = mean(values)
                )
                df = data.frame(variable = names(table), param = name, value = as.vector(table))
                df
            }
            statsDF = rbind(stats(posterior$$Local, "Local"), stats(posterior$$Company, "Company"))
			
			truth = data.frame(value = c(
				${Truth.parameters.spreadRates.local.rate},
				${Truth.parameters.spreadRates.company.rate}
			), variable = c("Local", "Company"))
			
			pdf("Link.density.$generationId%02d.pdf", width=8.26, height=2.91)
			ggplot(melt(posterior), aes(x = value, linetype = variable)) + 
				geom_density() +
				geom_vline(data = statsDF, aes(xintercept = value, linetype = param, colour = variable), show_guide = TRUE) +
				geom_vline(data = truth, aes(xintercept = value, linetype = variable)) +
				scale_x_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1))
			dev.off()
			pdf("Link.density.latest.pdf", width=8.26, height=2.91)
			ggplot(melt(posterior), aes(x = value, linetype = variable)) + 
				geom_density() +
				geom_vline(data = statsDF, aes(xintercept = value, linetype = param, colour = variable), show_guide = TRUE) +
				geom_vline(data = truth, aes(xintercept = value, linetype = variable)) +
				scale_x_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1))
			dev.off()
		"""
		ScriptRunner.apply(rScript, Data.wd.resolve("script.r"))
	}
		
	ABC(model, params, reporting)
}

object MetricTest extends App {
	val observedMatrix = Data.loadProportionOfTrueOutbreakData(1)
	val model = ScoringModel(observedMatrix)
	
	def getScores(params: Parameters): Seq[String] = {
		println(params)
		val samples = 100
		val meanDistanceDist = model.scoreDistribution(params)
		(1 to samples).par.map{_ => "%.5f".format(meanDistanceDist.sample)}.seq
	}
	
	val outFileLocal = Data.wd.resolve("metricTest_local.csv")
	val outFileCompany = Data.wd.resolve("metricTest_company.csv")
	
	val trueLocalRate = Truth.parameters.spreadRates.local.rate
	val trueCompanyRate = Truth.parameters.spreadRates.company.rate 
	
	CSV.writeLines(
		outFileLocal, 
		Seq(1,2,4,6,8,9).map{i => 
			Seq(i.toDouble) ++: getScores(Parameters(SpreadRates(i * 0.1, trueCompanyRate)))
		}.transpose
	)
	CSV.writeLines(
		outFileCompany, 
		Seq(1,2,4,6,8,9).map{i => 
			Seq(i.toDouble) ++: getScores(Parameters(SpreadRates(trueLocalRate, i  * 0.1)))
		}.transpose
	)

	val rScript = f"""
		lapply(c("ggplot2", "reshape", "hexbin"), require, character.only=T)
		
		localScores = read.csv("${outFileLocal.toAbsolutePath().toString}", check.names = F)
		companyScores = read.csv("${outFileCompany.toAbsolutePath().toString}", check.names = F)
		
		pdf("MetricTest.pdf", width=8.26, height=2.91) 
		ggplot(melt(localScores), aes(x = value, colour = variable)) + 
			geom_density() +
			ggtitle("Metric Score Distribution") + 
			scale_colour_discrete("Local\nparameter")
		ggplot(melt(companyScores), aes(x = value, colour = variable)) + 
			geom_density() + 
			ggtitle("Metric Score Distribution") + 
			scale_colour_discrete("Company\nparameter")
		dev.off()
	"""
	ScriptRunner.apply(rScript, Data.wd.resolve("script.r"))
}

object Data extends ToSamplable with Logging {
	val wd = Paths.get("results").resolve("Network")
	val obsPath = wd.resolve("diffMatrix.csv")
	
	def loadProportionOfTrueOutbreakData(proportionObserved: Double): DifferenceMatrix = {
		CSV.assertHeader(obsPath, "FarmA", "FarmB", "Diff")
		val csvLines = CSV.read(obsPath).toIndexedSeq
		
		val fullMatrix = DifferenceMatrix.fromCSV(csvLines.drop(1))
		val allInfected = fullMatrix.infectedFarms.toIndexedSeq

		val numToBeObserved = (allInfected.size * proportionObserved).toInt
		val observedFarms = allInfected.draw(numToBeObserved)(Random).drawnCounts.keySet + Truth.seedFarm
		val observedPartialMatrix = DifferenceMatrix(fullMatrix.cellMap.filterKeys{case (fidA, fidB) => 
			observedFarms.contains(fidA) && observedFarms.contains(fidB)
		})
	
		log.info(s"${fullMatrix.infectedFarms.size} true infections, ${observedPartialMatrix.infectedFarms.size} are used as observed")
		observedPartialMatrix
	}
}

class ABCLinksModel(observed: DifferenceMatrix) 
		extends Model[Parameters]
		with ToEmpirical {
	
	def perturb(p: Parameters) = Parameters.perturb(p)
	def perturbDensity(a: Parameters, b: Parameters) = Parameters.perturbDensity(a, b)
	val prior = Parameters.prior
	
	val scoringModel = ScoringModel(observed)
	def distanceToObservations(p: Parameters): Distribution[Double] = scoringModel.scoreDistribution(p)
}

case class ScoringModel(observed: DifferenceMatrix) extends ToSamplable with ToEmpirical {
	val obsInfecteds = observed.infectedFarms 
	
	val directDestByMech: Map[Int, Map[Int, Mechanism]] = obsInfecteds.map{centre =>
		val company = Network.companyExcludingSelf(centre)
		val localRemainder = Network.neighboursExcludingSelf(centre) -- company
		assert(!(company ++ localRemainder).contains(centre))
		val destMap: Map[Int, Mechanism] = (company.map{c => c -> CompanyTransmission} ++ localRemainder.map{l => l -> LocalTransmission}).toMap
		centre -> destMap
	}.toMap
	
	val observedDestinationsByMech: Map[Int, Map[Int, Mechanism]] = 
		obsInfecteds.map{centre =>
			val possDest = obsInfecteds - centre
			centre -> possDest.map{dest => dest -> directDestByMech(centre).get(dest)}
						.collect{case (dest, Some(mech)) => (dest, mech)}
						.toMap
		}.toMap
	
	case class ToFit(root: Int, mechanism: Mechanism, obsDiff: Int)
	val fitList = obsInfecteds.flatMap{root =>
		val destinationsByMech = observedDestinationsByMech(root)
				.groupBy{case (_, mech) => mech}
				.mapValues{_.map{case (id, _) => id}}
		val minDiffByMech = destinationsByMech.mapValues{leaves =>
			leaves.map{leaf => observed(root, leaf)}.min
		}		
		
		minDiffByMech.map{case (mechanism, minDiff) =>
			ToFit(
				root,
				mechanism,
				minDiff
			)
		}
	}
	fitList.foreach(println)
	
	def scoreDistribution(params: Parameters) = Distribution[Double]{
		val scores = fitList.toSeq.map{case ToFit(root, mechanism, obsDiff) =>
			val directDestinations: Set[Int] = directDestByMech(root)
				.collect{case (dest, `mechanism`) => dest}
				.toSet
				
			val simulatedSeqDifferences = (1 to 100).map{_ => OutbreakModel
				.simulateSequenceDifference(root, directDestinations, params)
				.toDouble
			}

			simulatedSeqDifferences
				.map(s => math.abs(s - obsDiff))
				.min
		}
		scores.sum / scores.size
	}
}

trait StopCondition{
	def apply(o: Outbreak, tick: Int): Boolean
}
case class SizeStopCondition(maxSize: Int) extends StopCondition with Logging {
	def apply(o: Outbreak, tick: Int) = {
		val stop = o.size >= maxSize
		stop
	}
}
case class CoveringStopCondition(observations: Set[Int]) extends StopCondition {
	def apply(o: Outbreak, tick: Int) = observations.forall(obs => o.infected.contains(obs))
}

object OutbreakModel {
	val transmissionReductionFactor = 0.01
	
	def simulateSequenceDifference(from: Int, to: Set[Int], p: Parameters): Int = {
		val localSpread = p.spreadRates.local.rate  * transmissionReductionFactor
		val companySpread = p.spreadRates.company.rate * transmissionReductionFactor
		
		def linkProb(dest: Int): Option[Double] = {
			if(Network.companyExcludingSelf(dest).contains(from)) Some(companySpread)
			else if(Network.neighboursExcludingSelf(dest).contains(from)) Some(localSpread)
			else None
		}
		
		val probs = to.toSeq.map{dest => linkProb(dest)}.flatten
		val failProbs = probs.map{1.0 - _}
		val probAllFail = failProbs.product
		
		def go(mutationsAcc: Int = 0): Int = {
			if(!Random.nextBoolean(probAllFail)) mutationsAcc + 1
			else go(mutationsAcc + 1)
		}
		
		go()
	}
	
	def simulateSequenceDifference(pair: (Int, Int), p: Parameters): Int = {
		val (from, to) =
			if(Random.nextDouble < 0.5) (pair._1, pair._2)
			else (pair._2, pair._1)
		
		val out = generate(p, from, CoveringStopCondition(Set(to)))
		Sequence.differenceCount(
				out.infectionMap(from).original, 
				out.infectionMap(to).original
		)
	}
	
	def generate(p: Parameters, seed: Int, stop: StopCondition, logging: Boolean = false): Outbreak = {
		val localSpread = p.spreadRates.local.rate  * transmissionReductionFactor
		val companySpread = p.spreadRates.company.rate * transmissionReductionFactor
		
		def addNewInfections(current: Outbreak): Outbreak = {
			val currentMap = current.infectionMap
			
			val infectionMap: Map[Int, Infection] = {
				currentMap.foldLeft(currentMap){case (acc, (iFid, infection)) =>
			 		val localNbrs = Network.neighboursExcludingSelf(iFid)
			 		val companyPrems = Network.companyExcludingSelf(iFid)
			 		
			 		val newCompanyTransmissions = Network.companyExcludingSelf(iFid)
			 			.filter(_ => Random.nextBoolean(companySpread))
			 			.filter(newInf => !acc.contains(newInf))
			 			
			 		val newLocalTransmissions = Network.neighboursExcludingSelf(iFid)
			 			.filter(_ => Random.nextBoolean(localSpread))
			 			.filter(newInf => !acc.contains(newInf) && !newCompanyTransmissions.contains(newInf))
			 			
			 		val newCompanyInfections = newCompanyTransmissions.map{farmId => 
		 				val mutated = Sequence.mutate(infection.current)
		 				farmId -> Infection(mutated, iFid, CompanyTransmission)
		 			}
			 		
			 		val newLocalInfections = newLocalTransmissions.map{farmId => 
		 				val mutated = Sequence.mutate(infection.current)
		 				farmId -> Infection(mutated, iFid, LocalTransmission)
		 			}
			 		
			 		if(logging){
			 			(newCompanyInfections ++ newLocalInfections).foreach{case (newId,newInf) =>
			 				println(s"New infection $iFid -> $newId, diff = ${Sequence.differenceCount(infection.original , newInf.original)}")
			 			}
			 		} 
			 		
			 		acc.++(newCompanyInfections ++ newLocalInfections)
				} 
			} 
				
			Outbreak(infectionMap, current.seed)	//TODO tidy
		}
		
		@tailrec def iterate(current: Outbreak, tick: Int): Outbreak = {
			if(logging) println(s"Tick $tick")
			if(stop(current, tick)) current
			else {
				val updated = addNewInfections(Outbreak.updateCurrentMutations(current))
				iterate(updated, tick + 1)
			}
		}
		
		iterate(Outbreak.setSeed(seed), 0)
	}
}

case class Outbreak(infectionMap: Map[Int, Infection], seed: Int){
	lazy val infected = infectionMap.keySet
	lazy val size = infectionMap.size
	lazy val differenceMatrix = DifferenceMatrix(this)
	
	def subsetBy(ids: Set[Int]) = Outbreak(
		infectionMap.filter{case (id, _) => ids.contains(id)},
		seed
	)
	
	def add(newInfections: Map[Int, Infection]): Outbreak = {
		val newInfectionsMap = newInfections.foldLeft(infectionMap){case (acc, (newId, newInfection)) => 
			if(infectionMap.contains(newId)) acc
			else acc + (newId -> newInfection)
		}
		
		Outbreak(newInfectionsMap, seed)
	}
}
object Outbreak{
	def setSeed(fid: Int) = {
		val freshVirusSequence = Sequence()
		Outbreak(Map(fid -> Infection(freshVirusSequence, freshVirusSequence, None)), fid)
	}
	def updateCurrentMutations(state: Outbreak) = Outbreak(
		state.infectionMap.map{case (id,infection) => id -> Infection.mutate(infection)},
		state.seed 
	)
}

case class DifferenceMatrix(cellMap: Map[(Int, Int), Int]){
	assert(!cellMap.keys.exists{case (farmA, farmB) => farmA <= farmB}, "Matrix entries muddled")
	
	lazy val infectedFarms = cellMap.keySet.foldLeft(Set.empty[Int]){case (acc, (a,b)) => acc + a + b}
	
	def apply(fidA: Int, fidB: Int): Int = {
		if(fidA < fidB) cellMap((fidB, fidA))
		else cellMap((fidA, fidB))
	}
	def toRows: Seq[Seq[Int]] = cellMap.map{case ((a,b), diff) => Seq(a,b,diff)}.toSeq
}
object DifferenceMatrix {
	def fromCSV(entries: Traversable[Traversable[String]]): DifferenceMatrix = {
		val map = entries.map{row =>
			val indexed = row.toIndexedSeq; assert(row.size == 3)
			val (a, b) = (indexed(0).toInt, indexed(1).toInt); assert(a > b)
			val diff = indexed(2).toInt
			((a ,b), diff)
		}.toMap
		DifferenceMatrix(map)
	}
	def apply(outbreak: Outbreak): DifferenceMatrix = {
		val sequenceMap: Map[Int, Sequence] = outbreak.infectionMap.map{case (fid, infection) => fid -> infection.original}.toMap
		
		val pairs = for{
			a <- 0 to Network.numFarms - 1
			b <- 0 until a
		} yield (a,b)
		
		def getDifference(aFid: Int, bFid: Int): Option[Int] = 
			for{
				seqA <- sequenceMap.get(aFid)
				seqB <- sequenceMap.get(bFid)
			} yield {
				Sequence.differenceCount(seqA, seqB)
			}
		
		val map = pairs.view.map{pair => 
				(pair, getDifference(pair._1, pair._2))
			}.collect{
				case (pair, Some(diff)) => (pair, diff)
			}.toMap
			
		DifferenceMatrix(map)
	}
}

case class Infection(current: Sequence, original: Sequence, source: Option[Source])
object Infection {
	def apply(sequence: Sequence, sourceId: Int, transmission: Mechanism): Infection = Infection(sequence, sequence, Some(Source(sourceId, transmission)))
	def mutate(i: Infection): Infection = Infection(Sequence.mutate(i.current), i.original, i.source)
}

case class Source(id: Int, mechanism: Mechanism)

trait Mechanism
object LocalTransmission extends Mechanism
object CompanyTransmission extends Mechanism

case class Sequence(basesInReverseOrder: IndexedSeq[Int] = IndexedSeq.empty){
	lazy val asIndexedSeq = basesInReverseOrder.toIndexedSeq.reverse
	lazy val numMutations = basesInReverseOrder.size
}
object Sequence{
	implicit val r = Random
	val randomBase = Distribution.uniform((1 to 4).toSeq)

	def mutate(s: Sequence) = Sequence(randomBase.sample +: s.basesInReverseOrder)
		
	def differenceCount(a: Sequence, b: Sequence): Int = { 
		val shortestLength = math.min(a.numMutations, b.numMutations)
		val longestLength = math.max(a.numMutations, b.numMutations)
		
		val firstDiff = a.asIndexedSeq.take(shortestLength).zip(b.asIndexedSeq.take(shortestLength)).indexWhere{case (a,b) => a != b}
		val result = 
			if(firstDiff < 0) longestLength - shortestLength
			else longestLength - firstDiff
				
		assert(result >= 0)
		result
	}
}

case class Parameters(spreadRates: SpreadRates){ 
	def toSeq = Seq(
			"%.7f".format(spreadRates.local.rate), 
			"%.7f".format(spreadRates.company.rate)
	)
}
object Parameters{
	val prior = new Prior[Parameters]{
		def density(p: Parameters) = {
			import p._
			SpreadRates.prior.density(spreadRates)
		}
		def sample = 
			Parameters(SpreadRates.prior.sample)
	}
	
	val names = Seq("Local", "Company")

	def perturb(p: Parameters) = Parameters(SpreadRates.perturb(p.spreadRates))
		
	def perturbDensity(a: Parameters, b: Parameters) =
		SpreadRates.perturbDensity(a.spreadRates, b.spreadRates)
}

object Network{
	val numFarms = 100
	
  	/*	10x10
	 * 
	 * 	9x			+					=	*	=
	 *  8x 		=			=	*	+			
	 *  7x						*	*	+		
	 *  6x		=		=						
	 *  5x		*	+							
	 *  4x			=		=		+		+	
	 *  3x										
	 * 	2x	*		*	+		+		+		
	 * 	1x		+		=						
	 * 	0x	+				#	+		*	=	
	 * 		0	1	2	3	4	5	6	7	8	9	
	 * 
	 * 		#:  Index case
	 * 		+:	0, 5, 11, 23, 25, 27, 46, 48, 52, 77, 86, 92
	 *   	=:	8, 13, 42, 44, 61, 63, 84, 97, 99
	 *    	*:	7, 20, 22, 51, 75, 76, 85, 98
	 */
	
//	val companyA = Set(0, 5, 11, 23, 25, 27, 46, 48, 52, 77, 86, 92)	// +
//	val companyB = Set(8, 13, 42, 44, 61, 63, 84, 97, 99)	// = 
//	val companyC = Set(7, 20, 22, 51, 75, 76, 85, 98)		// * 
	
  	/*	10x10 
	 * 
	 * 	9x			+							*
	 *  8x 					=	*				
	 *  7x										
	 *  6x										
	 *  5x			=							
	 *  4x									+	
	 *  3x										
	 * 	2x	*									
	 * 	1x				=						
	 * 	0x					#	+			=	
	 * 		0___1___2___3___4___5___6___7___8___9	
	 * 
	 */
	val companyA = Set(5, 48, 92)		// +
	val companyB = Set(8, 13, 52, 84)	// = 
	val companyC = Set(20, 85, 99)		// * 
	
	
	
	/* 
	 * 	9x	+									+
	 *  8x 	*								
	 *  7x										
	 *  6x										
	 *  5x					*	=				
	 *  4x										
	 *  3x										
	 * 	2x										
	 * 	1x				=	+					*
	 * 	0x	=				#					=
	 * 		0___1___2___3___4___5___6___7___8___9	
	 */
//	val companyA = Set(14, 90, 99)		// +
//	val companyB = Set(0, 13, 9, 55)	// = 
//	val companyC = Set(19, 54, 80)		// *
	
	def xyPos(fid: Int) = (fid % 10, fid / 10)
	
	def neighboursExcludingSelf(id: Int): Set[Int] = {
		assert(id < 100 && id >= 0,  s"Farm ID $id out of range")
		val (xPos, yPos) = xyPos(id)
		def axisNbrs(idx: Int): Set[Int] = idx match {
			case 0 => Set(0, 1)
			case 9 => Set(8, 9)
			case _ => Set(idx - 1, idx, idx + 1)
		}
		//Up/down/left/right & diagonal neighbours
		val includingSelf = for{
			x <- axisNbrs(xPos)
			y <- axisNbrs(yPos)
		} yield(x + 10 * y)
		includingSelf - id
		
		//Up-down-left-right only
//		val xVariation = axisNbrs(xPos).map(_ + 10 * yPos)
//		val yVariation = axisNbrs(yPos).map(_ * 10 + xPos)
//		val excludingSelf = xVariation ++ yVariation - id
//		excludingSelf
	}
	
	def companyExcludingSelf(id: Int) = {
		if(companyA.contains(id)) 		companyA - id
		else if(companyB.contains(id)) 	companyB - id
		else if(companyC.contains(id)) 	companyC - id
		else 							Set.empty[Int]
	}
}

case class SpreadRates(local: Transmission, company: Transmission)
object SpreadRates{
	def apply(local: Double, company: Double): SpreadRates = SpreadRates(Transmission(local), Transmission(company))
	
	val prior = new Prior[SpreadRates]{
		def density(r: SpreadRates) = {
			Transmission.prior.density(r.local) *
			Transmission.prior.density(r.company)
		}
		def sample = SpreadRates(Transmission.prior.sample, Transmission.prior.sample)
	}
	
	def perturb(r: SpreadRates) = SpreadRates(Transmission.perturb(r.local), Transmission.perturb(r.company))
		
	def perturbDensity(a: SpreadRates, b: SpreadRates): Double = {
		Transmission.perturbDensity(a.local, b.local) *
		Transmission.perturbDensity(a.company, b.company)
	}
}

case class Transmission(rate: Double) { assert(rate < 1 && rate > 0) }
object Transmission {
	case class UniformPrior(min: Double, max: Double){
		def density(x: Double) = if(x <= max && x >= min) 1.0/(max-min) else 0
		def sample = Random.nextDouble(min, max)
	}
	val uniformPrior = UniformPrior(0.0, 1.0)
	
	private val kernel = new Prior[Double] with Distribution[Double]{
		val normal = {
			val syncRand: RandomGenerator = new SynchronizedRandomGenerator(new MersenneTwister())
			new NormalDistribution(syncRand, 0, 0.05, NormalDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
		}
		def sample = normal.sample
		def density(at: Double) = normal.density(at)
	}
	
	def perturb(p: Transmission): Transmission = {
		def perturbUnit(u: Double): Double = {
			kernel.map(_ + u).filter(value => value <= 1.0 && value >= 0.0).sample
		}
		Transmission(perturbUnit(p.rate))
	}
	
	val prior = new Prior[Transmission]{
		def density(p: Transmission) = uniformPrior.density(p.rate)
		def sample = Transmission(uniformPrior.sample)
	}
	
	def perturbDensity(a: Transmission, b: Transmission): Double = kernel.density(a.rate - b.rate)
}