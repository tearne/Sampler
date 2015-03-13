package sampler.spike.farrington

import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Paths
import java.time.YearMonth

import scala.collection.SortedMap
import scala.io.Source

import org.json4s.DefaultFormats
import org.json4s.JObject
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL.int2jvalue
import org.json4s.JsonDSL.jobject2assoc
import org.json4s.JsonDSL.long2jvalue
import org.json4s.JsonDSL.pair2Assoc
import org.json4s.JsonDSL.seq2jvalue
import org.json4s.jvalue2extractable
import org.json4s.jvalue2monadic
import org.json4s.native.JsonMethods.compact
import org.json4s.native.JsonMethods.parse
import org.json4s.native.JsonMethods.pretty
import org.json4s.native.JsonMethods.render
import org.json4s.string2JsonInput
import org.rosuda.REngine.Rserve.RConnection
import sampler.spike.farrington.Farrington.Mode
import sampler.spike.farrington.Farrington.APHA
import sampler.spike.farrington.Farrington.Stl
import sampler.spike.farrington.Farrington.FarNew

case class Date(yearMonth: YearMonth, idx: Long)

case class Result(date: Date, actual: Int, expected: Double, threshold: Double, trend: Int, exceed: Double, weights: List[Double]){
	lazy val isAlert = actual > threshold
}
object Result{
	implicit val formats = DefaultFormats

	def apply(date: Date, actual: Int, json: JValue): Result = Result(
			date,			
			actual,
			(json \ "expected").extract[Double],
			(json \ "threshold").extract[Double],
			(json \ "trend").extract[Int],		
			(json \ "exceed").extract[Double],
			(json \ "weights").extract[List[Double]]
		)
}

object Farrington {
  trait Mode{
    val rFlag: String
  }
  case object FarNew extends Mode{
    val rFlag = "farNew"
  }
  case object APHA extends Mode{
    val rFlag = "apha"
  }
  case object Stl extends Mode{
    val rFlag = "stl"
  }
  
	/*
	 * TODO
	 * 
	 *  - Make more robust, returning a Try[Result] to gracefully handle problems in R
	 *  - An option to save/retrieve intermediate JSON to help with debugging of R
	 *    (could put something in the RServeHelper)
	 *  - Embed R script using """?
	 * 
	 */
	
	val cl = getClass.getClassLoader
	val rScript = Source.fromURI(cl.getResource("farrington/script.r").toURI()).mkString
	
	def run(dataIn: SortedMap[Date, Int], rCon: RConnection, mode: Mode = APHA): Result = {
		val json = buildJSON(dataIn)
	  val jsonAsString = pretty(render(json))
        
    //Debug
    val writer = Files.newBufferedWriter(Paths.get("outR.json"), Charset.defaultCharset())
    writer.write(jsonAsString)
    writer.close()
    
		val rExpression = {
      
			import rCon._
			parseAndEval("""library(rjson)""")
			assign("jsonIn", compact(render(json)))
      assign("modeFlag", mode.rFlag)
			parseAndEval("basedata = as.data.frame(fromJSON(jsonIn)$Baseline)")
     	parseAndEval("currentCount = as.data.frame(fromJSON(jsonIn)$Current$Incidents)")
      parseAndEval("currentmth = as.data.frame(fromJSON(jsonIn)$Current$Month)")
      parseAndEval("startdte = as.data.frame(fromJSON(jsonIn)$StartDate)")
			parseAndEval(rScript)
			parseAndEval("output")
		}
		
		val rOut = parse(rExpression.asString())
		
		val (date, value) = dataIn.last
		
		Result(date, value, rOut)
	}
	
	def buildJSON(timeSeries: SortedMap[Date, Int]): JObject = {
		val now = timeSeries.last
		val history = timeSeries
//      if (Mode == APHA) timeSeries.dropRight(1)
//      else timeSeries
    val firstDate = timeSeries.head._1
		
    val t = now._1.idx
    val r = now._2
    
    ("Current" -> 
      ("Month" -> now._1.idx) ~
      ("Incidents" -> now._2)
    ) ~
    ("Baseline" ->
      ("basemth" -> history.keySet.map(_.idx) ) ~
      ("basecont"-> history.values)
    ) ~
    ("StartDate" ->
      ("year" -> firstDate.yearMonth.getYear) ~
      ("month" -> firstDate.yearMonth.getMonthValue)
    )
	}
}