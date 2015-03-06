package sampler.spike.farrington

import java.nio.file.Files
import java.nio.file.Paths
import java.time.YearMonth

import scala.collection.SortedMap
import scala.io.Source

import org.json4s.DefaultFormats
import org.json4s.JField
import org.json4s.JObject
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL.int2jvalue
import org.json4s.JsonDSL.list2jvalue
import org.json4s.JsonDSL.long2jvalue
import org.json4s.JsonDSL.pair2Assoc
import org.json4s.JsonDSL.seq2jvalue
import org.json4s.JsonDSL.string2jvalue
import org.json4s.jvalue2extractable
import org.json4s.jvalue2monadic
import org.json4s.native.JsonMethods
import org.json4s.native.JsonMethods.compact
import org.json4s.native.JsonMethods.pretty
import org.json4s.native.JsonMethods.render
import org.json4s.string2JsonInput
import org.rosuda.REngine.Rserve.RConnection

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
	
	def run(dataIn: SortedMap[Date, Int], rCon: RConnection): Result = {
		val json = buildJSON(dataIn)
	  val jsonAsString = pretty(render(json))

		val rExpression = {
			import rCon._
			parseAndEval("""library(rjson)""")
			assign("jsonIn", compact(render(json)))
			parseAndEval("basedata = as.data.frame(fromJSON(jsonIn)$Baseline)")
     	parseAndEval("currentCount = as.data.frame(fromJSON(jsonIn)$Current$Incidents)")
      parseAndEval("currentmth = as.data.frame(fromJSON(jsonIn)$Current$Month)")
			parseAndEval(rScript)
			parseAndEval("output")
		}
		
		val rOut = JsonMethods.parse(rExpression.asString())
		
		val (date, value) = dataIn.last
		
		Result(date, value, rOut)
	}
	
	def buildJSON(timeSeries: SortedMap[Date, Int]): JObject = {
		val now = timeSeries.last
		val history = timeSeries.dropRight(1)
		
    ("Current" -> 
      ("Month" -> now._1.idx) ~
      ("Incidents" -> now._2)
    ) ~
    ("Baseline" ->
      ("basemth" -> history.keySet.map(_.idx) ) ~
      ("basecont"-> history.values)
    )
	}
}