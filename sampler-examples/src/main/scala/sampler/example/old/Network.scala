package sampler.example.old

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

case class SpreadRates(local: Double, company: Double){
	def toSeq = Seq(local, company)
}
object SpreadRates{
	def names = Seq("local", "company")
}

object NetworkModel extends Model[SpreadRates]{
	implicit val random = Random
	
 /*
  *  		0	1	2	3	4	5	6	7	8	9
  *    ---------------------------------------------
  * 	9|	.	.	.	.	.	.	.	.	.	.
  * 	8|	.	.	.	.	.	.	.	.	.	.
  * 	7|	.	B	B	.	.	.	.	A	.	.
  * 	6|	.	.	B	.	.	.	.	.	.	.
  * 	5|	.	.	.	.	.	.	.	.	.	.
  * 	4|	.	.	.	.	.	.	.	.	.	A
  * 	3|	.	.	.	.	.	.	.	I	.	.
  * 	2|	.	B	.	.	.	A	.	B	A	.
  * 	1|	.	.	.	.	.	.	.	.	.	.
  * 	0|	.	. 	.	.	.	B	.	A	.	.
  *  
  */	
	
	val companyA = Set(7, 25, 28, 49, 79)
	val companyB = Set(5, 21, 27, 62, 71, 72)
	
	val observations = Set(5, 10, 24, 37, 25, 14, 46, 57, 84, 61, 89, 6, 60, 28, 38, 21, 33, 13, 41, 73, 32, 17, 22, 27, 71, 49, 7, 80, 35, 48, 18, 16, 31, 72, 40, 26, 23, 8, 36, 30, 51, 19, 4, 79, 47, 15, 68, 62)
	val knownSource = Set(37)
	val runLengthDays = 14
	
	//TODO lots of scope for prior stuff in the core
	
	val prior = new Prior[SpreadRates]{
		val max = 0.2
		val min = 0.0
		def plausableRange(d: Double) = if(d > max || d < min) 0.0 else 1.0
		def density(p: SpreadRates) = plausableRange(p.local) * plausableRange(p.company) 
		def sample = SpreadRates(random.nextDouble(min, max), random.nextDouble(min, max))
	}
	
	//TODO introduce a perturbation kernel object?
	private val kernel = new Prior[Double] with Distribution[Double]{
		/*
		 * The Mersenne Twister is a fast generator with very good properties well suited for Monte-Carlo simulation
		 * http://commons.apache.org/proper/commons-math/userguide/random.html
		 */
		//TODO don't like the fact that there is another random at work here
		val normal = {
			val syncRand: RandomGenerator = new SynchronizedRandomGenerator(new MersenneTwister())
			new NormalDistribution(syncRand, 0, 0.01, NormalDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
		}
		def sample = {
			normal.sample
		}
		def density(at: Double) = {
			normal.density(at)
		}
	}
	
	def perturb(p: SpreadRates): SpreadRates = SpreadRates(
			p.local + kernel.sample,
			p.company + kernel.sample
	)
	def perturbDensity(a: SpreadRates,b: SpreadRates): Double = 
		kernel.density(a.local - b.local) *
		kernel.density(a.company - b.company)
		
	
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
		val fromCompanyA = if(companyA.contains(id)) companyA else Set.empty[Int] 
		val fromCompanyB = if(companyB.contains(id)) companyB else Set.empty[Int] 
		fromCompanyA ++ fromCompanyB
	}
		
	def infecteds(p: SpreadRates): Distribution[Set[Int]] = {
		val nodeIds = (0 until 100).toSet
		val localSpread = Distribution.bernouliTrial(p.local)
		val companySpread = Distribution.bernouliTrial(p.company)
		
		def nextAndCurrentInfections(current: Set[Int]): Set[Int] = {
			if(current.size == 100) current
			else current.flatMap{from =>
				val localNeighbours = neighboursIncludingSelf(from) - from
				val locallyInfected = localNeighbours.filter(_ => localSpread.sample)
				
				val companyNetwork = sameCompanyIncludingSelf(from) - from
				val companyInfected = companyNetwork.filter(_ => companySpread.sample)
				
				locallyInfected ++ companyInfected + from
			}
		}
		
		@tailrec
		def iterateDays(infected: Set[Int], daysRemaining: Int): Set[Int] = {
			if(daysRemaining == 0 || infected.size == 100) infected
			else {
				iterateDays(nextAndCurrentInfections(infected), daysRemaining - 1)
			}
		}
		
		Distribution{
			iterateDays(knownSource, runLengthDays)
		}
	} 
	
	def nodeAndSizeDiffmetric(simulated: Set[Int]) = 
		(simulated.diff(observations) union observations.diff(simulated)).size +
		math.abs(simulated.size - observations.size)
	def sizeDiffMetric(simulated: Set[Int]) = 
		math.abs(simulated.size - observations.size)
		
	def distanceToObservations(p: SpreadRates) = infecteds(p).map(nodeAndSizeDiffmetric)
}

object Generate extends App{
	val truth = SpreadRates(0.05,0.15)
	for(i <- 1 to 10) yield {
		val infected = NetworkModel.infecteds(truth).sample
		println(s"${infected.size}: $infected")
	}
}

object Network extends App{

	val wd = Paths.get("results").resolve("Network")
	Files.createDirectories(wd)

	val abcParams = ABCConfig.fromConfig(
		ConfigFactory.load,
		"network-example"
	)
	
	val abcReporting = { report: Report[SpreadRates] =>
		import report._
		
		val lines = SpreadRates.names +: posterior.map(_.toSeq)
		val fileName = f"posterior.$generationId%02d.csv"
		
		CSV.writeLines(
			wd.resolve(fileName),
			lines
		)
		
		val rScript = 
			f"""
			lapply(c("ggplot2", "reshape", "hexbin"), require, character.only=T)
			
			posterior = read.csv("$fileName")
			
			pdf("density.$generationId%02d.pdf", width=4.13, height=2.91) #A7 landscape paper
			ggplot(posterior, aes(local, company)) +
				stat_binhex() +
				scale_x_continuous(limits = c(0,0.2)) +
				scale_y_continuous(limits = c(0,0.2))
			
			ggplot(posterior, aes(x = local)) + 
				geom_density() +
				scale_x_continuous(limits = c(0,0.2))
			ggplot(posterior, aes(x = company)) + 
				geom_density() +
				scale_x_continuous(limits = c(0,0.2))
			dev.off()
				"""
		ScriptRunner.apply(rScript, wd.resolve("script.r"))
			
	}
		
	ABC(NetworkModel, abcParams, abcReporting)
}