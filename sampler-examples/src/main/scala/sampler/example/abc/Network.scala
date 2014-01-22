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

case class SpreadRates(local: Double, company: Double){
	def toSeq = Seq(local, company)
}
object SpreadRates{
	def names = Seq("local", "company")
}

object NetworkModel extends Model[SpreadRates]{
	implicit val random = Random
	val observations = Set(56, 64, 44, 54, 43, 55)
	val knownSource = Set(44)

	//TODO lots of scope for prior stuff in the core
	
	val prior = new Prior[SpreadRates]{
		def unitRange(d: Double) = if(d > 1.0 || d < 0.0) 0.0 else 1.0
		def density(p: SpreadRates) = unitRange(p.local) * unitRange(p.company) 
		def sample = SpreadRates(random.nextDouble, random.nextDouble)
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
			new NormalDistribution(syncRand, 0, 0.1, NormalDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
		}
		def sample = {
			val r = normal.sample
			if(r.isNaN() || r.isInfinite()) {
				val e = new Exception("here... r = "+r)
				e.printStackTrace()
				throw e
			}
			r
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
		val runLengthDays = 30
		
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
				case i if id % 7 != 0 || id % 23 != 0 => Set.empty[Int]
				case i if id % 7 == 0 => nodeIds.filter(_ % 7 == 0 )
				case i if id % 23 == 0 => nodeIds.filter(_ % 23 == 0 )
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
		
	def distanceToObservations(p: SpreadRates) = infecteds(p).map(sizeDiffMetric)
}

object Test extends App{
	val p = SpreadRates(0.1,0.01)
	(1 to 1000).foreach{i => 
		val inf = NetworkModel.infecteds(p).sample
		val dist = NetworkModel.sizeDiffMetric(inf)
		println(s"$i: $dist, $inf")
	}
}

object NetworkABC extends App{

	val wd = Paths.get("results").resolve("Network")
	Files.createDirectories(wd)

	val abcParams = ABCConfig.fromConfig(
		ConfigFactory.load,
		"network-example"
	)
	val posterior = ABC(NetworkModel, abcParams)
	
	val lines = SpreadRates.names +: posterior.map(_.toSeq)
	CSV.writeLines(
		wd.resolve("fitted.csv"),
		lines
	)
	
	val rScript = 
s"""
lapply(c("ggplot2", "reshape", "hexbin"), require, character.only=T)

posterior = read.csv("fitted.csv")

pdf("density.pdf", width=4.13, height=2.91) #A7 landscape paper
ggplot(posterior, aes(local, company)) + stat_binhex()
dev.off()
"""
	ScriptRunner.apply(rScript, wd.resolve("script.r"))
	
}