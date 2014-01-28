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


/*
 * 
 * Population grid showing company A & B locations
 * 
 * 	.	A	.	.	.	.	.	.	A	B(99)
 *  .	.	.	.	A	.	.	.	B	.
 * 	A	.	.	.	.	.	.	AB	.	.
 * 	.	.	.	A	.	.	B	.	.	.
 *  .	.	.	.	.	B	A	.	.	.
 *  .	.	A	.	B	.	.	.	.	A
 *  .	.	.	B(33)	A	.	.	.	.
 *  .	A	B	.	.	.	.	.	A	.
 *  .	B	.	.	A	.	.	.	.	.
 *  AB(0)	.	.	.	.	.	A	.	.
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
	val observations = Set(0, 88, 10, 56, 42, 14, 78, 84, 28, 70, 21, 33, 53, 77, 32, 34, 22, 44, 12, 49, 7, 98, 91, 66, 80, 35, 63, 11, 43, 99, 55, 23, 79, 68)
	val knownSource = Set(33)
	val runLengthDays = 14
	
	//TODO lots of scope for prior stuff in the core
	
	val prior = new Prior[SpreadRates]{
		val max = 0.15
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
		
	def infecteds(p: SpreadRates): Distribution[Set[Int]] = {
		val nodeIds = (0 until 100).toSet
		val localSpread = Distribution.bernouliTrial(p.local)
		val colleagueSpread = Distribution.bernouliTrial(p.company)
		
		def neighbours(id: Int): Set[Int] = {
			val (yPos, xPos) = (id / 10, id % 10)
			def axisNbrs(idx: Int): Set[Int] = idx match {
				case 0 => Set(0, 1)
				case 9 => Set(8, 9)
				case _ => Set(idx - 1, idx, idx + 1)
			}
			val includingSelf = for{
				x <- axisNbrs(xPos)
				y <- axisNbrs(yPos)
			} yield(x + y * 10)
			includingSelf - id
		}
	
		def colleagues(id: Int): Set[Int] = {
			val includingSelf = id match{
				case i if id % 7 != 0 && id % 11 != 0 => Set.empty[Int]
				case i if id % 7 == 0 => nodeIds.filter(_ % 7 == 0)
				case i if id % 11 == 0 => nodeIds.filter(_ % 11 == 0)
			}
			includingSelf - id
		}
		
		def nextAndCurrentInfections(current: Set[Int]): Set[Int] = {
			if(current.size == 100) current
			else current.flatMap{infected =>
				val locallyInfected = neighbours(infected).filter(_ => localSpread.sample)
				val companyInfected = colleagues(infected).filter(_ => colleagueSpread.sample)
				locallyInfected ++ companyInfected + infected
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
	val truth = SpreadRates(0.01,0.09)
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
				scale_x_continuous(limits = c(0,0.15)) +
				scale_y_continuous(limits = c(0,0.15))
			
			ggplot(posterior, aes(x = local)) + 
				geom_density() +
				scale_x_continuous(limits = c(0,0.15), breaks = c(0,0.01,0.15), labels = c(0,0.1,0.15))
			ggplot(posterior, aes(x = company)) + 
				geom_density() +
				scale_x_continuous(limits = c(0,0.15), breaks = c(0,0.09,0.15))
			dev.off()
				"""
		ScriptRunner.apply(rScript, wd.resolve("script.r"))
			
	}
		
	ABC(NetworkModel, abcParams, abcReporting)
}