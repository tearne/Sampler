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

case class Parameters(localTransmission: Double, companyTransmission: Double){
	def toSeq: Seq[Any] = Seq(localTransmission, companyTransmission)
}
object Parameters {
	implicit val r = Random
	
	val names = Seq("LocalTransmission", "CompanyTransmission")
	
	val farmIdRange = new {
		val min = 0
		val max = 99
	}
	val priorTransmissionSupport = new {
		val min = 0.0
		val max = 0.2
	}
	val allFarmIds = (farmIdRange.min to farmIdRange.max).toSet
	
	def position(fid: Int) = (fid / 10, fid % 10)
	
	def neighboursIncludingSelf(id: Int): Set[Int] = {
		assert(id < 100 && id >= 0,  "Farm ID out of range")
		val (yPos, xPos) = position(id)
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
			new NormalDistribution(syncRand, 0, 0.001, NormalDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
		}
		def sample = normal.sample
		def density(at: Double) = normal.density(at)
	}
	
	def perturbUnit(u: Double): Double = {
		kernel.map(_ + u).filter(value => value <= 1.0 && value >= 0.0).sample
	}
	
	def perturb(p: Parameters): Parameters = {
		import p._
		Parameters(
				perturbUnit(localTransmission),
				perturbUnit(companyTransmission)
		)
	}
	
	val prior = new Prior[Parameters]{
		def transRateDensity(d: Double) = if(d > priorTransmissionSupport.max || d < priorTransmissionSupport.min) 0.0 else 1.0
		def density(p: Parameters) = {
			import p._
			transRateDensity(localTransmission) * transRateDensity(companyTransmission)
		}
		def sample = Parameters(
			r.nextDouble(priorTransmissionSupport.min, priorTransmissionSupport.max), 
			r.nextDouble(priorTransmissionSupport.min, priorTransmissionSupport.max)
		)
	}
	
	def perturbDensity(a: Parameters, b: Parameters): Double = {
		kernel.density(a.localTransmission - b.localTransmission) *
		kernel.density(a.companyTransmission - b.companyTransmission)
	}
}

object NetworkModel extends Model[Parameters]{
 /*
  *  		0	1	2	3	4	5	6	7	8	9
  *    ---------------------------------------------
  * 	9|	.	.	.	.	.	.	.	.	.	.
  * 	8|	.	.	.	.	.	.	.	.	.	.
  * 	7|	.	B	B	.	.	.	I	A	.	.
  * 	6|	.	.	B	.	.	.	.	.	.	.
  * 	5|	.	.	.	.	.	.	.	.	.	.
  * 	4|	.	.	.	.	.	.	.	.	.	A
  * 	3|	.	.	.	.	.	.	.	.	.	.
  * 	2|	.	B	.	.	.	A	.	B	A	.
  * 	1|	.	.	.	.	.	.	.	.	.	.
  * 	0|	.	. 	.	.	.	B	.	A	.	.
  *  
  */	
	
	val companyA = Set(7, 25, 28, 49, 79)
	val companyB = Set(5, 21, 27, 62, 71, 72)
	
	implicit val random = Random
	val sourceFarm = 76
	val observations = Outbreak(Set(56, 57, 65, 77, 86, 76, 95, 67, 87, 75, 58),Set(Transmission(76,86), Transmission(67,58), Transmission(67,57), Transmission(76,75), Transmission(86,95), Transmission(86,87), Transmission(86,77), Transmission(67,56), Transmission(75,65), Transmission(76,67)))
	val runLengthDays = 14
	
	def perturb(p: Parameters) = Parameters.perturb(p)
	def perturbDensity(a: Parameters, b: Parameters) = Parameters.perturbDensity(a, b)
	val prior = Parameters.prior
		
	def infecteds(p: Parameters): Distribution[Outbreak] = {
		val localSpread = Distribution.bernouliTrial(p.localTransmission)
		val companySpread = Distribution.bernouliTrial(p.companyTransmission)
		
		def addNewInfections(current: Outbreak): Outbreak = {
			if(current.infected.size == 100) current
			else {
				val transmissions: Set[Transmission] = current.infected.flatMap{from =>
					val localNeighbours = Parameters.neighboursIncludingSelf(from) - from
					val locallyInfected = localNeighbours.filter(_ => localSpread.sample)
					
					val companyNetwork = Parameters.sameCompanyIncludingSelf(from) - from
					val companyInfected = companyNetwork.filter(_ => companySpread.sample)
					
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
			iterateDays(Outbreak.seed(sourceFarm), runLengthDays)
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
		
		val r1 = simu.toSeq.map(simuTrans => if(real.contains(simuTrans)) 0 else 1 )
		val r2 = real.toSeq.map(realTrans => if(simu.contains(realTrans)) 0 else 1 )
		val result = (r1 ++ r2).sum
//		println(result+ "   -      "+r1+" ++ "+r2)
		result
	}
//		
//	def sizeDiffMetric(simulated: Outbreak) = 
//		math.abs(simulated.numInfected - observations.numInfected)
//		
	def distanceToObservations(p: Parameters): Distribution[Double] = {
		infecteds(p).map(outbreak => nodeDiffmetric(outbreak))
	}
}

object Generate extends App{
	val truth = Parameters(0.05,0.15)
	for(i <- 1 to NetworkModel.runLengthDays) yield {
		val infected = NetworkModel.infecteds(truth).sample
		println(infected)
	}
}

object Network extends App{

	val wd = Paths.get("results").resolve("Network")
	Files.createDirectories(wd)

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
				scale_x_continuous(limits = c(0,0.2), breaks = c(0,0.05,0.2))

			ggplot(posterior, aes(x = CompanyTransmission)) + 
				geom_density() +
				scale_x_continuous(limits = c(0,0.2), breaks = c(0,0.1,0.2))
			
			ggplot(posterior, aes(LocalTransmission, CompanyTransmission)) +
				stat_binhex() +
				scale_x_continuous(limits = c(0,0.2)) +
				scale_y_continuous(limits = c(0,0.2))
			
			dev.off()
		"""
		ScriptRunner.apply(rScript, wd.resolve("script.r"))
			
	}
		
	ABC(NetworkModel, abcParams, abcReporting)
}