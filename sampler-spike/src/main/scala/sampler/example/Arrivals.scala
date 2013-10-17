/*
* Copyright (c) Oliver Tearne (tearne at gmail dot com)
*
* This program is free software: you can redistribute it and/or modify it under the terms of
* the GNU General Public License as published by the Free Software Foundation, either version
* 3 of the License, or (at your option) any later version.
*
* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
* See the GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License along with this program.
* If not, see <http://www.gnu.org/licenses/>.
*/

package sampler.example
import collection.immutable.Queue
import sampler.math.Random
import java.nio.file.Paths
import java.nio.file.Files
import sampler.math.Probability
import scala.annotation.tailrec
import sampler.r.ScriptRunner
import sampler.data.Distribution
import org.apache.commons.math3.distribution.NormalDistribution
import sampler.Implicits._
import sampler.io.CSVFile

/*
 * TODO
 * 
 *  - use for-comprehensions more
 *  - Output graph of queue size against time with confidence interval
 *  - Don't bother with optimisation
 */

object Arrivals extends App{
	import ArrivalsUtils._
	
	val wd = Paths.get("results", "Arrivals")
	Files.createDirectories(wd)
	
	/*
	 * Build distributions
	 */
	val planeTimetable = {
		val t: Time = 30.minutes
		val peakArrivals = IndexedSeq(
				normalTime(6.hours, 30.minutes),	// 6 AM
				normalTime(9.hours, 30.minutes),	// 9 AM
				normalTime(19.hours, 2.hours)		// 7 PM
		)
		Distribution
			.uniform(peakArrivals)(r)
			.map(_.sample)
	}
	val wind = normalDist(0,1).filter(strength => math.abs(strength) < 3)
			
	val planeArrivalDistribution = for{
		numPlanesToday	  	<- Distribution.uniform(20, 100)
		actualArrivalTimes	<- planeTimetable
				.combine(wind)((p,w) => (p.inSeconds + 60 * w).toInt.seconds)
				.until(_.size == numPlanesToday)
	} yield actualArrivalTimes
	
	def numHallArrivalsPerFlight(landingTime: Time) = for {
		numPassengers		<- Distribution.uniform(80, 300)
		walkToImmigration	<- normalTime(10.minutes, 10.minutes).until(_.size == numPassengers)
		hallArrivalTimes	= walkToImmigration.map(_ + landingTime).groupBy(identity).mapValues(_.size)
		arrivalsBySecond	= (1 to Time.dayInSeconds).map(m => hallArrivalTimes.getOrElse(m.seconds, 0)) 
	} yield arrivalsBySecond
	
	val todaysImmigrationArrivals = new Distribution[IndexedSeq[Int]]{
		def sample() = {
			val todaysPlanes = planeArrivalDistribution.sample
			todaysPlanes.map(landingTime => numHallArrivalsPerFlight(landingTime).sample).foldLeft(IndexedSeq.fill(Time.dayInSeconds)(0)){case (acc, planeLoad) =>
				acc.zip(planeLoad).map{case (a,b) => a + b}
			}
		}
	}
	
	//Assume all staff work 3-1-3 shifts
	val staffing = Staffing(Seq(
			Staff(0),Staff(0),Staff(1),Staff(1),
			Staff(5),Staff(5),Staff(5),Staff(5),Staff(5),Staff(5),Staff(6),Staff(6),
			Staff(11),Staff(11),Staff(12),Staff(12),
			Staff(16),Staff(16),Staff(16),Staff(16),
			Staff(8)
	))
	
	val processingTime = {
		val quick = normalTime(20.seconds, 5.seconds)
		val slow = normalTime(2.minutes, 30.seconds)
		val fivePercent = Probability(0.05)
		Distribution.bernouliTrial(fivePercent).map{isSlow =>
			if(isSlow) slow.sample else quick.sample
		}
	}
	
	class QSize(staffing: Staffing, arrivalsSamplable: Distribution[IndexedSeq[Int]]) extends Distribution[Seq[Int]]{
		/*
		 * Sample a sequence of ints, representing the length of the queue for every second in the day
		 */
		def sample() = {
			go(	Nil,
				arrivalsSamplable.sample,
				Time(0), 
				Time(1), 
				Queue[Int](), 
				Nil
			)
		}
		
		@tailrec
		private def go(
				workerWorkload: Seq[Int], 
				arrivals: IndexedSeq[Int], 
				prev: Time, 
				now: Time, 
				q: Queue[Int], 
				qHistory: Seq[Int]	//The size of the queue at each second
		): Seq[Int] = {
			//Update recording of queue length
			val timeDiff = now.inSeconds - prev.inSeconds
			val qHistory1 = if(timeDiff > 1){
				Seq.fill(timeDiff)(q.size) ++: qHistory
			} else  q.size +: qHistory
			
			//Update workforce size in response to staff arriving/leaving/
			//assuming that staff will finish the job they are currently on
			//when ending a short
			val workerStatus1 = if(prev.hour < now.hour){
				val workforceDelta = staffing.levelAt(now.hour) - workerWorkload.size
				workforceDelta match{
					case 0 => workerWorkload
					case d if d < 0 => workerWorkload.take(-d)
					case d if d > 0 => workerWorkload ++: Seq.fill(d)(0)
				}
			} else workerWorkload
			
			//Update the workforce busy times given time passed
			val workerStatus2 = workerStatus1.map(t => if(t == 0) 0 else t - timeDiff)
			
			//Add new arrivals to the Q
			val numNewArrivals = arrivals.slice(prev.inSeconds + 1, now.inSeconds + 1).sum
			val q1 = q.enqueue{
				(0 until numNewArrivals).map(_ => processingTime.sample.inSeconds)
			}
			
			//Allocate arrivals to free staff
			val (workerStatus3, q2) = workerStatus2.allocate(_ == 0, q1)
			
			//Determine how far ahead in time to jump
			val nextTime = 
				math.min(
					now.inSeconds + math.max(1, if(workerStatus3.size == 0) 1 else workerStatus3.min), 
					Time.dayInSeconds - 1
				).seconds
			
			if(nextTime.inSeconds == Time.dayInSeconds - 1) qHistory1.reverse
			else go(workerStatus3, arrivals, now, nextTime, q2, qHistory1)
		}
	}
	
	val getHourlyQueueSize = new Distribution[IndexedSeq[Int]]{
		def sample = {
			val result = new QSize(staffing, todaysImmigrationArrivals).sample
			val qMaxByHour = result.grouped(1.hours.inSeconds).map(_.max).toIndexedSeq
			qMaxByHour
		}
	}
	
	val numRuns = 10
	val queueSamples = (1 to numRuns).map(_ => getHourlyQueueSize.sample)
	
	val longFormatResults = {
		case class Row(sim: Int, hour: Int, qSize: Int){
			def toCSV = s"$sim, $hour, $qSize"
		}
		
		for{
			sim <- (0 until numRuns)
			(qSize, time) <- queueSamples(sim).zipWithIndex
		} yield Row(sim, time, qSize)
	}
	
	val outPath = wd.resolve("out.csv")
	
	CSVFile.write(
		outPath, 
		longFormatResults.map(_.toCSV), 
		header = Seq("Sim", "Hour", "QSize")
	)
	
	ScriptRunner(s"""
lapply(c("ggplot2", "reshape"), require, character.only=T)

data = read.csv("${outPath.getFileName()}")
data$$Sim = factor(data$$Sim)

pdf("plot.pdf", width=5.8, height=4.1) #A7 landscape paper
ggplot(data, aes(x=Hour, y=QSize, colour=Sim))+geom_line()
dev.off()			
""",
				wd.resolve("script") //TODO file extension?
		)
}

object ArrivalsUtils{
	implicit val r = Random
	
	class Time(val inSeconds: Int) extends AnyVal{
		import Time._
		def hour: Int = (inSeconds / (60.0 * 60.0)).toInt
		def minutes: Int = (inSeconds / 60.0).toInt
		def +(that: Time) = new Time(inSeconds + that.inSeconds)
		def next = Time(inSeconds + 1)
		override def toString() = s"Time($inSeconds)"
	}
	object Time{
		val dayInSeconds = 24 * 60 * 60
		def apply(seconds: Int) = new Time(moduloDay(seconds))
		
		private def moduloDay(s0: Int) = {
			val s = s0 % dayInSeconds
			if(s < 0) s + dayInSeconds else s
		}
	}
	
	implicit class RicherDouble(d: Double){
		def seconds = Time(d.toInt)
		def minutes = Time((d * 60).toInt)
		def hours = Time((d * 60 * 60).toInt)
	}
	
	implicit class RicherInt(i: Int){
		def seconds = Time(i)
		def minutes = Time(i * 60)
		def hours = Time(i * 60 * 60)
	}
	
	implicit class RichSeq[T](seq: Seq[T]){
		def allocate(condition: T => Boolean, from: Queue[T]): (Seq[T], Queue[T]) = {
			@tailrec
			def go(acc: Seq[T], remaining: Seq[T], q: Queue[T]): (Seq[T], Queue[T]) = {
				if (q.isEmpty || remaining.isEmpty) (acc ++ remaining, q)
				else {
					val (accu, rem) = remaining.span(e => !condition(e))
					if (rem.isEmpty) (acc ++ accu, q)
					else go(acc ++ accu.:+(q.head), rem.tail, q.tail)
				}
			}
			go(Nil, seq, from)
		}
	}
	
	def normalDist(mean: Double, sd: Double) = {
		val normal = new NormalDistribution(mean,sd)
		Distribution(normal.sample)
	}
	
	def normalTime(mean: Time, sd: Time): Distribution[Time] = 
		normalDist(mean.inSeconds, sd.inSeconds).filter(_ > 0).map(_.toInt.seconds)
		
	
	case class Staff(startHr: Int){
		assert(startHr >= 0 && startHr < 24)
		def willBeWorkingAt(hr: Int) = {
			(hr >= startHr && hr < startHr + 3) ||
			(hr >= startHr + 4 && hr < startHr + 7) 
		}
	}
		
	case class Staffing(staff: Seq[Staff]){
		def levelAt(hr: Int) = {
			staff.count(_.willBeWorkingAt(hr))
		}
	}
}

