package sampler.spike.farrington

import java.nio.file.{Files,Paths}
import java.time.YearMonth
import java.time.temporal.ChronoUnit.MONTHS
import scala.annotation.elidable.ASSERTION
import scala.collection.immutable.TreeMap
import scala.io.Source
import scala.language.postfixOps
import org.json4s.DefaultFormats
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.rosuda.REngine.Rserve.RConnection
import sampler.r.rserve.RServeHelper

object EDS extends App{
	val resultsDir = Paths.get("results", "farrington")
	Files.createDirectories(resultsDir)
	
	val cl = getClass.getClassLoader
	val rScript = Source.fromURI(cl.getResource("farrington/script.r").toURI()).mkString
	val input = cl.getResource("farrington/input.txt").toURI()
	
	implicit val formats = DefaultFormats
	
	val dataIn = TreeMap{
		Source.fromFile(input)
			.getLines()
			.filter(_.trim.size > 0)
			.zipWithIndex
			.map{case (line, idx) =>
				val toks = line.split("\t").map(_.trim.toInt)
				val year = toks(0)
				val month = toks(1)
				val incidentCount = toks(3)
				YearMonth.of(year, month) -> incidentCount
			}
			.toSeq: _*
	}
	
	assert(!dataIn.exists(_._1.getYear == 2001)) 
	val first = dataIn.head._1
	val last = dataIn.last._1
	val window = List(-1, 0, 1).map(v => (v + 12) % 12)
	
	def isBaseline(date: YearMonth) = {
		def isWithinWindow() = {
			val monthRemainder = MONTHS.between(date, last) % 12
			val inWindow = window.exists(_ == monthRemainder)
      val inYrRangeMin = MONTHS.between(date, last) <= 12*12+1
      val inYrRangeMax = MONTHS.between(date, last) > 2
      inWindow && inYrRangeMin && inYrRangeMax
		}
		date.getYear != 2001 && isWithinWindow
	}
	
	val (bDates, bCounts) = dataIn.filterKeys(isBaseline).unzip	
  val (cDate, cCount) = dataIn.last
  
  bDates.zip(bCounts).foreach(println)
  println(s"Current = $cDate, $cCount")
	
  //TODO change first?
  
	val json = 
    ("Current" -> 
      ("Month" -> MONTHS.between(first, cDate)) ~
      ("Incidents" -> cCount)
    ) ~
    ("Baseline" ->
      ("basemth" -> bDates.map{d => MONTHS.between(first, d)}) ~
      ("basecont"-> bCounts)
    )
    
  val jsonAsString = pretty(render(json))
  val writer = Files.newBufferedWriter(Paths.get("tmp.json"))
  writer.write(jsonAsString)
  writer.newLine
  writer.close
    
  println(jsonAsString)
	
	RServeHelper.ensureRunning()
	val r = new RConnection
	val rExp = 
		try{
			r.parseAndEval("""library(rjson)""")
			r.assign("jsonIn", compact(render(json)))
			r.parseAndEval("basedata = as.data.frame(fromJSON(jsonIn)$Baseline)")
      r.parseAndEval("currentCount = as.data.frame(fromJSON(jsonIn)$Current$Incidents)")
      r.parseAndEval("currentmth = as.data.frame(fromJSON(jsonIn)$Current$Month)")
			r.parseAndEval(rScript)
			r.parseAndEval("output")
		}finally{
			r.close
			RServeHelper.shutdown
		}
	
	val rOut = parse(rExp.asString())
	println("------- JSON OUT -------")
	println(pretty(render(rOut)))
	println("--------- END ----------")

case class MyRes(expected: Double, threshold: Double, trend: Int, exceed: Double, weights: List[Double])
	val res = rOut.extract[MyRes]
	println(res)
}