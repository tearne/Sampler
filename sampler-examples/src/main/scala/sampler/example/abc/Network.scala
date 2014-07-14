package sampler.example.abc

import sampler.data.Distribution
import sampler.math.Random
import scala.collection.immutable.ListMap
import sampler.cluster.abc.Model
import sampler.cluster.abc.Prior
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.distribution.NormalDistribution
import java.nio.file.Paths
import java.nio.file.Files
import sampler.cluster.abc.ABC
import sampler.cluster.abc.config.ABCConfig
import com.typesafe.config.ConfigFactory
import sampler.io.CSV
import sampler.r.ScriptRunner
import scala.annotation.tailrec
import sampler.cluster.abc.actor.Report
import scala.language.reflectiveCalls

/*
 * An outbreak starts in the centre of a 10 x 10 grid of 
 * nodes and after two weeks all the infected nodes are 
 * observed.  Infection travels between neighbouring nodes
 * with probability 'local' and amongst company premises 
 * with probability 'company'.  Transmission probabilities
 * are evaluated once per day per infected farm.
 * 
 * Company A premises have ids divisable by 7, and company 
 * B premises are divisable by 11. 
 * 
 * Given knowledge of where outbreak started (id = 44) and
 * where it spread to within two weeks, determine the 
 * 'local' and 'company' transmission rates.  
 * 
 */

case class Transmission(from: Int, to: Int){
	assert(from < 100 && from >= 0)
	assert(to < 100 && to >= 0)
	lazy val reversed = Transmission(to, from)
}

case class Outbreak(infected: Set[Int], events: Set[Transmission]){
	def add(transmissions: Set[Transmission]): Outbreak = 
		Outbreak(infected ++ transmissions.map(_.to), events ++ transmissions)
	lazy val numInfected = infected.size
}
object Outbreak{
	def seed(fid: Int) = Outbreak(Set(fid), Set.empty[Transmission])
}

//case class InfectionDates(map: Map[Int, Int]){
//	def add(farms: Set[Int], day: Int): InfectionDates = {
//		farms.foldLeft(this){case (acc, farm) => acc.add(farm, day)}
//	}
//	def add(farm: Int, day: Int): InfectionDates = {
//		assert(!infecteds.contains(farm))
//		InfectionDates(map + (farm -> day))
//	}
//	def dayInfected(farm: Int): Int = map(farm)
//	lazy val infecteds = map.keySet
//	lazy val numInfected = map.size
//}
//object InfectionDates{
//	def seed(farm: Int) = InfectionDates(Map(farm -> 0))
//}

case class UniformPrior(min: Double, max: Double){
	def density(x: Double) = if(x <= max && x >= min) 1.0/(max-min) else 0
	def sample(r: Random) = r.nextDouble(min, max)
}

case class Parameters(localTransmission: Double, companyTransmission: Double, sourceFarm: Int){
	def toSeq: Seq[Any] = {
		val sourcePos = Parameters.position(sourceFarm)
		Seq(localTransmission, companyTransmission, sourcePos._1, sourcePos._2)
	}
}
object Parameters {
	implicit val r = Random
	
	val names = Seq("LocalTransmission", "CompanyTransmission", "SourceX", "SourceY")
	
	val farmIdRange = new {
		val min = 0
		val max = 99
	}
	val localSpreadPrior = UniformPrior(0.0,0.5)
	val companySperadPrior = UniformPrior(0.0,0.5)
	
	val allFarmIds = (farmIdRange.min to farmIdRange.max).toSet
	
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
		
//		val xNbrs = axisNbrs(xPos).map(x => x + 10 * yPos)
//		val yNbrs = axisNbrs(yPos).map(y => xPos + 10 * y)
//		xNbrs ++ yNbrs
		
//		Set(id)
	}
	
	val sqrt200 = math.sqrt(200)
	def distanceFn(from: Int, to: Int): Double = {
		val fromPosition = Parameters.position(from)
		val toPosition = Parameters.position(to)
		val dist = math.sqrt(math.pow(fromPosition._1 - toPosition._1, 2) + math.pow(fromPosition._2 - toPosition._2, 2))
		(sqrt200 - dist) / sqrt200
	}
	
	def sameCompanyIncludingSelf(id: Int): Set[Int] = {
		import NetworkModel.{companyA, companyB}
		val fromCompanyA = if(companyA.contains(id)) companyA else Set.empty[Int] 
		val fromCompanyB = if(companyB.contains(id)) companyB else Set.empty[Int] 
		fromCompanyA ++ fromCompanyB
	}
	
	private val kernel = new Prior[Double] with Distribution[Double]{
		/*
		 * The Mersenne Twister is a fast generator with very good properties well suited for Monte-Carlo simulation
		 * http://commons.apache.org/proper/commons-math/userguide/random.html
		 */
		//TODO don't like the fact that there is another random at work here
		val normal = {
			val syncRand: RandomGenerator = new SynchronizedRandomGenerator(new MersenneTwister())
			new NormalDistribution(syncRand, 0, 0.1, NormalDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
		}
		def sample = normal.sample
		def density(at: Double) = normal.density(at)
	}
	
	def perturbUnit(u: Double): Double = {
		kernel.map(_ + u).filter(value => value <= 1.0 && value >= 0.0).sample
	}

	def oneStepAway(fid: Int): Set[Int] = {
		val nbrs = Parameters.neighboursIncludingSelf(fid)
		val company = Parameters.sameCompanyIncludingSelf(fid)
		nbrs ++ company
	}
	
	def perturb(p: Parameters): Parameters = {
		import p._
		Parameters(
				perturbUnit(localTransmission),
				perturbUnit(companyTransmission),
				Distribution.uniform(oneStepAway(sourceFarm).toIndexedSeq).sample
		)
	}
	
	val prior = new Prior[Parameters]{
		def density(p: Parameters) = {
			import p._
			localSpreadPrior.density(localTransmission) * companySperadPrior.density(companyTransmission) / 100.0
		}
		def sample = Parameters(
			localSpreadPrior.sample(r), 
			companySperadPrior.sample(r),
			Distribution.uniform(allFarmIds.toIndexedSeq).sample
		)
	}
	
	def perturbDensity(a: Parameters, b: Parameters): Double = {
		val aNbrs = oneStepAway(a.sourceFarm)
		val spatialDensity = if(aNbrs.contains(b.sourceFarm)) 1.0 / aNbrs.size else 0
		
		
		kernel.density(a.localTransmission - b.localTransmission) *
		kernel.density(a.companyTransmission - b.companyTransmission) *
		spatialDensity
	}
}

object Test extends App{
	println(Parameters.position(27))
	println(Parameters.neighboursIncludingSelf(27))
}

object Generate extends App{
	val source = 83
	val truth = Parameters(0.05,0.25, source)
	for(i <- 1 to NetworkModel.runLengthDays) yield {
		val infected = NetworkModel.infecteds(truth).sample
		println(infected)
	}
}
object NetworkModel extends Model[Parameters]{
  /*
  *  		0	1	2	3	4	5	6	7	8	9
  *    ---------------------------------------------
  * 	9|	.	.	.	.	.	.	.	.	B	A
  * 	8|	.	.	.	B	.	.	A	.	.	.
  * 	7|	.	A	.	.	.	.	.	.	.	.
  * 	6|	.	.	.	.	.	.	.	.	.	.
  * 	5|	.	.	A	.	.	.	.	.	.	.
  * 	4|	.	.	.	.	.	.	.	.	.	A
  * 	3|	.	.	.	.	.	.	.	.	.	.
  * 	2|	.	.	.	.	A	A	.	A	.	.
  * 	1|	.	A	.	.		B	.	.	.	.
  * 	0|	.	. 	.	.	.	A	.	.	.	.
  *  
  */	
	
	val companyA = Set(5, 11, 24, 25, 27, 49, 52, 71, 86, 99)
	val companyB = Set(15, 83, 98)
	
	implicit val random = Random
	val observations = Outbreak(Set(5, 24, 25, 52, 93, 29, 84, 89, 28, 97, 73, 27, 71, 49, 86, 98, 35, 95, 11, 72, 99, 82, 94, 15, 83),Set(Transmission(52,49), Transmission(27,52), Transmission(83,82), Transmission(83,98), Transmission(52,11), Transmission(94,84), Transmission(83,15), Transmission(71,25), Transmission(83,93), Transmission(71,27), Transmission(94,95), Transmission(83,72), Transmission(24,11), Transmission(49,99), Transmission(99,25), Transmission(52,5), Transmission(83,84), Transmission(27,28), Transmission(99,86), Transmission(99,11), Transmission(25,52), Transmission(94,93), Transmission(52,25), Transmission(27,25), Transmission(25,24), Transmission(99,27), Transmission(98,97), Transmission(25,35), Transmission(15,83), Transmission(11,27), Transmission(27,49), Transmission(83,94), Transmission(98,89), Transmission(27,71), Transmission(49,5), Transmission(28,29), Transmission(52,27), Transmission(83,73), Transmission(98,99)))
	val runLengthDays = 7
	
	def perturb(p: Parameters) = Parameters.perturb(p)
	def perturbDensity(a: Parameters, b: Parameters) = Parameters.perturbDensity(a, b)
	val prior = Parameters.prior
		
	def infecteds(p: Parameters): Distribution[Outbreak] = {
		val localSpread = Distribution.bernoulliTrial(p.localTransmission)
		val companySpread = Distribution.bernoulliTrial(p.companyTransmission)
		
		def addNewInfections(current: Outbreak): Outbreak = {
			if(current.infected.size == 100) current
			else {
				val transmissions: Set[Transmission] = current.infected.flatMap{from =>
					val localNeighbours = Parameters.neighboursIncludingSelf(from) - from
					val locallyInfected = localNeighbours.filter(_ => localSpread.sample)
					
					val companyPrems = (Parameters.sameCompanyIncludingSelf(from) - from).toIndexedSeq
					val companyProbs = companyPrems.map(to => Parameters.distanceFn(from, to) * p.companyTransmission)
					val companyInfected = companyPrems.zip(companyProbs).filter{case (prem, prob) => random.nextDouble < prob}.map(_._1)

					(locallyInfected ++ companyInfected).map(to => Transmission(from, to))
				}
				
				current.add(transmissions)
			}
		}
		
		@tailrec
		def iterateDays(current: Outbreak, daysLeft: Int): Outbreak = {
			if(daysLeft == 0 || current.numInfected == 100) current
			else {
				iterateDays(addNewInfections(current), daysLeft - 1)
			}
		}
		
		Distribution{
			iterateDays(Outbreak.seed(p.sourceFarm), runLengthDays)
		}
	} 
	
//	def euclideanDistance(simulated: Set[Int]) = {
//		def distSquared(farmAId: Int, farmBId: Int): Double = {
//			def distSq(a: (Int, Int), b: (Int, Int)): Double = {
//				math.pow(b._1 - a._1, 2) + math.pow(b._2 - a._2, 2)
//			}
//			
//			distSq(Parameters.position(farmAId), Parameters.position(farmBId))
//		}
//		
//		
//		def setDistanceSquared(setA: Set[Int], setB: Set[Int]) = 
//			setA.map{a => setB.map{b =>distSquared(a, b)}.min
//		}.max
//		
//		setDistanceSquared(observations, simulated) + setDistanceSquared(simulated, observations)
//	}
		
//	def dayDifference(simulated: InfectionDates) = {
//		val common = (simulated.infecteds intersect observations.infecteds)
//		val notCommon = (simulated.infecteds.diff(observations.infecteds) union observations.infecteds.diff(simulated.infecteds))
//		
//		val dayDiffs = common.foldLeft(0){case (acc, fid) => acc + math.abs(simulated.dayInfected(fid) - observations.dayInfected(fid))}
//		dayDiffs + notCommon.size * NetworkModel.runLengthDays
//	}
	
	def nodeDiffmetric(simulated: Outbreak) = 
		(simulated.infected.diff(observations.infected) union observations.infected.diff(simulated.infected)).size

		
	def transmissionRoutes(simulated: Outbreak) = {
		val simu = simulated.events
		val real = observations.events
		
		def score(t:Transmission, events: Set[Transmission]): Int = {
			if(events.contains(t.reversed)) 100
			else if(!events.contains(t)) 10
			else 0
		}		
		
		val r1 = simu.map(score(_,real)).sum
		val r2 = real.map(score(_,simu)).sum
//		println(result+ "   -      "+r1+" ++ "+r2)
		r2 + r1
	}
	
	def transmissionDirection(sim: Outbreak) = {
		val obsEvnts = observations.events
		val simEvnts = sim.events
		val a = obsEvnts.map(evnt => if(simEvnts.contains(evnt.reversed)) 1 else 0).sum
		val b = simEvnts.map(evnt => if(obsEvnts.contains(evnt.reversed)) 1 else 0).sum
		a + b
	}
	
	def sizeDiffMetric(simulated: Outbreak) = 
		math.abs(simulated.numInfected - observations.numInfected)
		
	def distanceToObservations(p: Parameters): Distribution[Double] = {
		infecteds(p).map(outbreak => transmissionDirection(outbreak) + nodeDiffmetric(outbreak) + sizeDiffMetric(outbreak))
	}
}

object Network extends App {

	val wd = Paths.get("results").resolve("Network")
	Files.createDirectories(wd)

	val abcParams = ABCConfig.fromTypesafeConfig(
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

			ggplot(posterior, aes(x = CompanyTransmission)) + 
				geom_density() +
				scale_x_continuous(limits = c(0,0.5))
			
			ggplot(posterior, aes(LocalTransmission, CompanyTransmission)) +
				stat_binhex() +
				scale_x_continuous(limits = c(0,0.5)) +
				scale_y_continuous(limits = c(0,0.5))
			
			ggplot(posterior, aes(SourceX, SourceY)) +
				xlim(0,10) + ylim(0,10) +
				geom_bin2d(binwidth  = c(0.999,0.999))
			
			dev.off()
		"""
		ScriptRunner.apply(rScript, wd.resolve("script.r"))
			
	}
		
	ABC(NetworkModel, abcParams, abcReporting)
}