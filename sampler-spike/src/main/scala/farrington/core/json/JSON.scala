package farrington.core.json

import scala.collection.SortedMap
import org.json4s.JObject
import org.json4s.native.JsonMethods.compact
import org.json4s.native.JsonMethods.parse
import org.json4s.native.JsonMethods.pretty
import org.json4s.native.JsonMethods.render
import org.json4s.JsonDSL.int2jvalue
import org.json4s.JsonDSL.jobject2assoc
import org.json4s.JsonDSL.long2jvalue
import org.json4s.JsonDSL.pair2Assoc
import org.json4s.JsonDSL.seq2jvalue
import farrington.core.result.Date
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Path

object JSON {
  
  def buildTimeSeriesJSON(timeSeries: SortedMap[Date, Int]): JObject = {
    val current = timeSeries.last
    val history = timeSeries
    val startDate = timeSeries.head._1
    
    ("Current" -> 
      ("Month" -> current._1.idx) ~
      ("Incidents" -> current._2)
    ) ~
    ("Baseline" ->
      ("basemth" -> history.keySet.map(_.idx) ) ~
      ("basecont"-> history.values)
    ) ~
    ("StartDate" ->
      ("year" -> startDate.yearMonth.getYear) ~
      ("month" -> startDate.yearMonth.getMonthValue)
    )
  }
  
  def writeJSON(json: JObject, path: Path) = {    
    val jsonAsString = pretty(render(json))
    val writer = Files.newBufferedWriter(path, Charset.defaultCharset())
    writer.write(jsonAsString)
    writer.close()
  }

}