///*
//* Copyright (c) Oliver Tearne (tearne at gmail dot com)
//*
//* This program is free software: you can redistribute it and/or modify it under the terms of
//* the GNU General Public License as published by the Free Software Foundation, either version
//* 3 of the License, or (at your option) any later version.
//*
//* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
//* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//* See the GNU General Public License for more details.
//*
//* You should have received a copy of the GNU General Public License along with this program.
//* If not, see <http://www.gnu.org/licenses/>.
//*/
//
//package sampler.example
//
//import collection.immutable.Queue
//import sampler.data.Samplable
//import sampler.math.Random
//import java.nio.file.Paths
//import java.nio.file.Files
//import sampler.r.QuickPlot
//import sampler.math.Probability
//import scala.annotation.tailrec
//import sampler.data.Types.Column
//import sampler.io.CSVTableWriter
//import sampler.r.ScriptRunner
//
//object Arrivals extends App{
//	import ArrivalsUtils._
//	
//	val wd = Paths.get("results", "Arrivals")
//	Files.createDirectories(wd)
//	
//	val timetable = {
//		val t: Time = 30.minutes
//		val peakArrivals = List(
//				normalTime(6.hours, 30.minutes),	// 6 AM
//				normalTime(9.hours, 30.minutes),	// 9 AM
//				normalTime(19.hours, 2.hours)		// 7 PM
//		)
//		Samplable
//			.uniform(peakArrivals)
//			.map(_.sample)
//	}
//	
//	val wind = Samplable
//		.normal(0, 1)
//		.filter(strength => math.abs(strength) < 3)
//	val flightsPerDay = Samplable.uniform(20, 100)
//	val passengersPerFlight = Samplable.uniform(80, 300)
//	val walkingTime = normalTime(10.minutes, 10.minutes)
//			
//	val flightArrivalsTimes = timetable.combine(wind)((time, wind) => 
//		(time.inMinutes + wind * 60).toInt.minutes
//	)
//		
//	val immigrationHallArrivals = new Samplable[IndexedSeq[Int]]{
//		def sample() = {
//			val numPlanesToday = flightsPerDay.sample
//			val planeArrivals = flightArrivalsTimes.until(_.size == numPlanesToday).sample
//			val arrivalHallTimes = 
//				planeArrivals.flatMap{planeTime =>
//					Seq.fill(passengersPerFlight.sample)(walkingTime.sample + planeTime)
//				}
//				.groupBy(identity)
//				.mapValues(_.size)
//			val arrivalsByMinute = 
//				(1 to Time.dayInMinutes)
//				.map(i => arrivalHallTimes.getOrElse(i.minutes, 0))
//			arrivalsByMinute
//		}
//	}	
//	
//	QuickPlot.writeDensity(
//		wd, 
//		"FlightArrivalDistribution", 
//		Map(
//			"Due" -> Seq.fill(1e5.toInt)(timetable.sample.toDouble),
//			"Actual" -> Seq.fill(1e5.toInt)(flightArrivalsTimes.sample.toDouble)
//		)
//	)
//	
//	//Assume all staff work a 3,1,3 hour shifts
//	
//	
//	
//	val staffing = Staffing(Seq(
//			Staff(0),Staff(0),Staff(1),Staff(1),
//			Staff(5),Staff(5),Staff(6),Staff(6),
//			Staff(11),Staff(11),Staff(12),Staff(12),
//			Staff(16),Staff(16),Staff(16),Staff(16),
//			Staff(8)
//	))
//	
////	val currentStaff = Array.fill(staffing.numWorkingAt(0))(0)
////	val arrivals = immigrationHallArrivals.sample
//	
//	val processingTime = {
//		val quick = normalTime(0.2.minutes, 0.1.minutes)
//		val slow = normalTime(2.minutes, 0.5.minutes)
//		val fivePercent = Probability(0.05)
//		Samplable.bernouliTrial(fivePercent).map{isSlow =>
//			if(isSlow) slow.sample else quick.sample
//		}
//	}
//	
//	class QSize(staffing: Staffing, arrivalsSamplable: Samplable[IndexedSeq[Int]]) extends Samplable[Seq[Int]]{
//		def sample() = {
//			go(	Nil,
//				arrivalsSamplable.sample,
//				Time(0), 
//				Time(1), 
//				Queue[Int](), 
//				Nil
//			)
//		}
//		
//		@tailrec
//		private def go(workerStatus: Seq[Int], arrivals: IndexedSeq[Int], prev: Time, now: Time, q: Queue[Int], qHistory: Seq[Int]): Seq[Int] = {
//			//Update recording of queue length
//			val timeDiff = now.inSeconds - prev.inSeconds
//			val qHistory1 = if(timeDiff > 1){
//				Seq.fill(timeDiff)(q.size) ++: qHistory
//			} else  q.size +: qHistory
//			
//			//Update workforce size
//			val workerStatus1 = if(prev.hour < now.hour){
//				val workforceDelta = staffing.numWorkingAt(now.hour) - workerStatus.size
//				workforceDelta match{
//					case 0 => workerStatus
//					case d if d < 0 => workerStatus.take(-d)
//					case d if d > 0 => workerStatus ++: Seq.fill(d)(0)
//				}
//			} else workerStatus
//			
//			//Update the workforce busy times given time passed
//			val workerStatus2 = workerStatus1.map(t => if(t == 0) 0 else t - timeDiff)
//			
//			//Add new arrivals to the Q
//			val numNewArrivals = arrivals.slice(prev.inSeconds + 1, now.inSeconds + 1).sum
//			val q1 = q.enqueue{
//				(0 until numNewArrivals).map(_ => processingTime.sample.inSeconds)
//			}
//			
//			//Allocated arrivals to free staff
//			val (workerStatus3, q2) = workerStatus2.allocate(_ == 0, q1)
//			
//			//Determine how far ahead in time to jump
//			val nextTime = 
//				math.min(
//					now.inSeconds + math.max(1, if(workerStatus3.size == 0) 1 else workerStatus3.min), 
//					Time.dayInSeconds - 1
//				).seconds
//			
//			if(nextTime.inSeconds == Time.dayInSeconds - 1) qHistory1.reverse
//			else go(workerStatus3, arrivals, now, nextTime, q2, qHistory1)
//		}
//	}
//	
//	val result = new QSize(staffing, immigrationHallArrivals).sample
//	val qMaxByMinute = result.grouped(1.minutes.inSeconds).map(_.max).toList
//	val qData = Column(qMaxByMinute, "Q")
//	
//	new CSVTableWriter(wd.resolve("out.csv"), true)(
//			Column((0 until Time.dayInMinutes), "Time"),
//			//arrivalsData, 
//			qData)
//	
//	val dataPath = wd.resolve("out.csv").toAbsolutePath().toString
//	println(dataPath)
//	
//	ScriptRunner(s"""
//lapply(c("ggplot2", "reshape"), require, character.only=T)
//
//data = read.csv("$dataPath")
//
//pdf("plot.pdf", width=5.8, height=4.1) #A7 landscape paper
//ggplot(melt(data, id="Time"), aes(x=Time, y=value, colour=variable))+geom_line()
//dev.off()			
//""",
//				wd.resolve("script") //TODO file extension?
//		)
//		
//		
//	//////////////////////////////
//	val reps = 1
//	def getEnergy(s: Staffing): Double = {
//		//Energy is the mean maximum Q size in N runs
//		val samp = new QSize(s, immigrationHallArrivals)
//		Iterator.fill(reps)(samp.sample.max).sum / reps.toDouble
//	}	
//		
//	val start = Staffing.generate(5)
//	val energy = getEnergy(start)
//	println("Here")
//	
//	val steps = 10
//	def temperature(r: Double) = 1000*r //Sort this out
//	def accepted(currentEnergy: Double, proposedEnergy: Double, temp: Double) =
//		if(proposedEnergy < currentEnergy) true
//		else {
//			val p = Probability(math.exp(-(proposedEnergy - currentEnergy)/temp))
//			println("P = "+p)
//			r.nextBoolean(p)
//		}
//	
//	def go(staffing: Staffing, step: Int, energy: Double): Staffing = {
//		println(staffing)
//		if(step == steps) staffing
//		else{
//			val temp = temperature(step.toDouble / steps)
//			val nhbr = staffing.randomNeighbour
//			val enrg = getEnergy(nhbr)
//			if(accepted(energy, enrg, temp)){
//				println("Accepted")
//				go(nhbr, step+1, enrg)
//			} 
//			else {
//				println("Rejected")
//				go(staffing, step+1, energy)
//			}
//		}
//	}
//	
//	go(start, 0, energy)
//}
//
//object ArrivalsUtils{
//	implicit val r = Random
//	
//	class Time private (val inMinutes: Int) extends AnyVal{
//		import Time._
//		def hour: Int = inMinutes / 60
//		def +(that: Time) = new Time(inMinutes + that.inMinutes)
//		def next = Time(inMinutes + 1)
//		def toDouble = inMinutes.toDouble
//		override def toString() = s"Time($inMinutes)"
//	}
//	object Time{
//		val dayInMinutes = 24 * 60
//		def apply(seconds: Int) = new Time(moduloDay(seconds))
//		
//		private def moduloDay(s0: Int) = {
//			val s = s0 % dayInMinutes
//			if(s < 0) s + dayInMinutes else s
//		}
//	}
//	
//	implicit class RicherInt(n: Int){
//		def minutes = Time(n * 60)
//		def hours = Time(n * 60 * 60)
//	}
//	
//	implicit class RichSeq[T](seq: Seq[T]){
//		def allocate(condition: T => Boolean, from: Queue[T]): (Seq[T], Queue[T]) = {
//			@tailrec
//			def go(acc: Seq[T], remaining: Seq[T], q: Queue[T]): (Seq[T], Queue[T]) = {
//				if (q.isEmpty || remaining.isEmpty) (acc ++ remaining, q)
//				else {
//					val (accu, rem) = remaining.span(e => !condition(e))
//					if (rem.isEmpty) (acc ++ accu, q)
//					else go(acc ++ accu.:+(q.head), rem.tail, q.tail)
//				}
//			}
//			go(Nil, seq, from)
//		}
//	}
//	
//	def normalTime(mean: Time, vari: Time) = Samplable
//		.normal(mean.inMinutes, vari.inMinutes)
//		.map(_.toInt.minutes)
//		
//	
//	case class Staff(startHr: Int){
//		assert(startHr >= 0 && startHr < 24)
//		def willBeWorkingAt(hr: Int) = {
//			(hr >= startHr && hr < startHr + 3) ||
//			(hr >= startHr + 4 && hr < startHr + 7) 
//		}
//	}
//		
//	case class Staffing(staff: Seq[Staff]){
//		def numWorkingAt(hr: Int) = {
//			staff.count(_.willBeWorkingAt(hr))
//		}
//		def randomNeighbour = {
//			val r = Random
//			val index = r.nextInt(staff.size)
//			val plusOrMinus = if(r.nextBoolean) 1 else -1
//			val newStartHour = {
//				val hr = (staff(index).startHr + plusOrMinus) % 24
//				if(hr < 0) hr + 24 else hr
//			}
//			println(staff(index).startHr + " ->" + newStartHour)
//			Staffing(staff.updated(index, Staff(newStartHour)))
//		}
//	}
//	object Staffing{
//		implicit val r = Random
//		def generate(numStaff: Int) = {
//			val samplable = Samplable.uniform(0, 24)
//			Staffing(Seq.fill(numStaff)(Staff(samplable.sample)))
//		}
//	}
//}
//
