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

package sampler.examples

import collection.immutable.Queue
import sampler.data.Samplable
import sampler.math.Random
import java.nio.file.Paths
import java.nio.file.Files
import sampler.r.QuickPlot
import sampler.math.Probability
import scala.annotation.tailrec

object Arrivals extends App{
	implicit val r = Random
	val wd = Paths.get("egout", "Arrivals")
	Files.createDirectories(wd)
	
	//TODO make a numeric time object, or use Duration
	
	class Time private (val seconds: Int) extends AnyVal{
		import Time._
//		def completeHours = Time(hourInSeconds * (seconds / hourInSeconds))
		def hour = seconds / hourInSeconds
		def + (that: Time) = new Time(this.seconds + that.seconds)
		def next = Time(seconds + 1)
		override def toString() = s"Time($seconds)"
	}
	object Time{
		val hourInSeconds = 60 * 60
		val dayInSeconds = hourInSeconds * 24
		
		def apply(seconds: Int) = new Time(moduloDay(seconds))
		
		private def moduloDay(s0: Int) = {
			val s = s0 % dayInSeconds
			if(s < 0) s + dayInSeconds else s
		}
	}
	
	implicit class RicherInt(n: Int){
		def seconds = Time(n)
		def minutes = Time(n * 60)
		def hours = Time(n * 60 * 60)
	}
	
	implicit def timeToSecondsDouble(t: Time) = t.seconds.toDouble
	
	def normal(mean: Time, vari: Time) = Samplable.normal(mean.seconds, vari.seconds).map(_.toInt.seconds)
	
	val timetable = {
		val peakArrivals = List(
				normal(6.hours,30.minutes),	// 6 AM
				normal(9.hours,30.minutes),	// 9 AM
				normal(19.hours,2.hours)	// 7 PM
		)
		Samplable
			.uniform(peakArrivals)
			.map(_.sample)
	}
	
	val wind = Samplable
			.normal(0, 1)
			.filter(strength => math.abs(strength) < 3)

	val flightsPerDay = Samplable
			.uniform(10, 15)
			.map(_.toInt)
	
	val passengersPerFlight = Samplable.uniform(80, 200)
	
	val walkingTime = Samplable
			.normal(10.minutes, 10.minutes)
			.map(_.toInt.seconds)
			
	val planeTimes = timetable
		.combine(wind)((time, wind) => (time.seconds + wind * 60).toInt.seconds)
		
	// Each sample generates passenger arrival times at 
	//the immigration hall for a whole day
	val arrivalsBySecond = new Samplable[Seq[Int]]{
		def sample() = {
			val numPlanesToday = flightsPerDay.sample
			val flightArrivalTimes = planeTimes.until(_.size == numPlanesToday).sample
			println("arrival times = "+flightArrivalTimes)
			val arrivalHallTimes = flightArrivalTimes.flatMap{planeTime =>
				Seq.fill(passengersPerFlight.sample)(walkingTime.sample + planeTime)
			}.groupBy(identity)//.groupBy(_.completeHours)
			.mapValues(_.size)
			val arrivalsBySecond = (1 to Time.dayInSeconds).map(i => arrivalHallTimes.getOrElse(i.seconds, 0))
			arrivalsBySecond
		}
	}	
	
//	System.exit(0)
	
	//TODO passenger arrival stream
	//TODO immigration queue
		
		
//	QuickPlot.writeDensity(
//			wd, 
//			"arrivalDist", 
//			Map(
//					"due" -> (0 to 100000).map(_ => timetable.sample),
//					"arrival" -> (0 to 100000).map(_ => arrivals.sample)
//			)
//	)
	
	/*
	 * Assume immigration staff work in 6 hour shifts
	 */
	
	case class Staffing(numWorkersByHour: Seq[Int]){
		assert(numWorkersByHour.size == 24)
		def apply(hr: Int) = numWorkersByHour(hr)
	}
//	class Availability(val seconds: Int) extends AnyVal
	
	val staffing = Staffing(Seq(
			5, 5, 5, 5, 5, 5,
			5, 5, 5, 5, 5, 5,
			5, 5, 5, 5, 5, 5,
			5, 5, 5, 5, 5, 5
	))
	val workforce = Array.fill(staffing(0))(0)
	val arrivals = arrivalsBySecond.sample
	
	println(arrivals)
//	System.exit(0)	
	
	implicit class RichSeq[T](seq: Seq[T]){
		def patch(condition: T => Boolean, from: Queue[T]): (Seq[T], Queue[T]) = {
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
	
		val processingTime = {
		Samplable.uniform(1, 10).map(_.toInt)
		val quick = Samplable.normal(10.seconds, 2.seconds).map(t => math.abs(t).toInt)
		val slow = Samplable.normal(30.seconds, 10.seconds).map(t => math.abs(t).toInt)
		val tenPercent = Probability(0.1)
		Samplable.bernouliTrial(tenPercent).map{isSlow =>
			if(isSlow) slow.sample else quick.sample
		}
	}
	
	//Input: initial staffing level, time and queue size.  
	//Output: queue size at every second of the day
	def go(workforce: Seq[Int], prev: Time, now: Time, waitingQ: Queue[Int], qSizeAcc: Seq[Int]): Seq[Int] = {

		println("")
		println("now = "+now)
		
		//Update recording of queue length
		val timeDiff = now.seconds - prev.seconds
		println("timeDiff "+timeDiff)
		val qSizeAcc0 = if(timeDiff > 1){
			//We've jumped ahead
			qSizeAcc ++: Seq.fill(timeDiff - 1)(waitingQ.size)
		} else qSizeAcc :+ waitingQ.size
		println("qsizeAcc0 "+qSizeAcc0.size)
		
		//Update workforce size
		val workforce0 = if(prev.hour < now.hour){
			val workforceDelta = staffing(now.hour) - workforce.size
			workforceDelta match{
				case 0 => workforce
				case d if d < 0 => workforce.take(d)
				case d if d > 0 => workforce ++: Seq.fill(d)(0) //d new staff, all available now
			}
		} else workforce
		println("Workforce0 "+workforce0)
		
		//Update staff 'busy' times and allocate work to those available
		val numNewArrivals = arrivals.slice(prev.seconds + 1, now.seconds + 1).sum
		println("numNewArrivals "+numNewArrivals)
		
		println("waitingQ "+waitingQ)
		val waitingQ1 = waitingQ.enqueue{
			(0 to numNewArrivals).map(_ => processingTime.sample)
		}
		println("waitingQ1 "+waitingQ1)
		
		//Use the implicit!
		val (workforce1, waitingQ2) = RichSeq(workforce0).patch(_ <= 0, waitingQ)
		
		//Determine how far ahead to jump
		val next = now + math.max(1, workforce1.min).seconds
		println("Advance to next "+next+s" $now + ${math.max(1, workforce1.min).seconds}")
		
		if(next.seconds >= Time.dayInSeconds) qSizeAcc0.reverse
		else go(workforce1, now, next, waitingQ2, qSizeAcc0)
	}
	
	println(go(workforce, Time(0), Time(1), Queue[Int](), Nil))
	
//	case class Worker(shiftStarted: Int, busyFor: Int){
//		def isWorking(time: Int) = shiftStarted <= time && time < shiftStarted + 6.hours
//		def tick = copy(busyFor = this.busyFor - 1)
//		def isFree = busyFor <= 0
//		def setBusyFor(time: Int) = Worker(shiftStarted, time)
//	}
//	
//	def workers(startTimeMins: Int, number: Int) = Seq.fill(number)(Worker(startTimeMins, 0)) 
//	
//	val workers: IndexedSeq[Worker] = {
//		workers(0.hours,50000) ++:
//		workers(5.hours,100) ++:
//		workers(10.hours,50) ++:
//		workers(16.hours,50) ++:
//		workers(20.hours,30) ++:
//		Nil
//	}.toIndexedSeq
//	
//	val arrivalsHall = Queue[Int]()
//	

//	
////	println((0 to 40).foreach(_ => println(processingTime.sample)))
////	System.exit(0)
//	
//	case class State(waiting: Queue[Int], workers: IndexedSeq[Worker], time: Int)
//	object State{
//		def init = State(Queue[Int](), workers, 0)
//	}
//	
//	val arrivalTimes = arrivalHallTimes.sample
//	println("Number of passengers today "+arrivalTimes.values.size)
////	println(" -- "+arrivals.take(100))
//	def tick(state: State): State = {
//		println("Time = "+state.time)
//		println("workers free = "+{state.workers.filter(_.isFree).size})
//		val t = arrivalTimes.getOrElse(state.time, 0)
//		println("Num arrivals "+t)
//		val newArrivals = (0 until t).map(_ => processingTime.sample)
//		println("New arrivals = "+newArrivals.size)
//		
//		val s = state.copy(
//				time = state.time + 1, 
//				waiting = state.waiting.enqueue(newArrivals)
//		)
//		
//		state.workers.zipWithIndex.foldLeft(s){case (s, (w, i)) =>
//			if(state.waiting.size == 0 || !w.isWorking(state.time) || !w.isFree) 
//				s.copy(workers = s.workers.updated(i, w.tick))
//			else {
//				val (processingTime, newQ) = s.waiting.dequeue
//				s.copy(
//						waiting = newQ, 
//						workers = s.workers.updated(i, w.setBusyFor(processingTime)))
//			}
//		}
//	}
//	
//	def go(acc: State, t: Int = 0): State = {
//		println(acc.waiting.size)
//		if(t == 24.hours) acc
//		else go(tick(acc), t + 1)
//	}
//	
//	go(State.init)
}




