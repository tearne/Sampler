package farrington.core.result

import java.time.YearMonth
import org.json4s.JsonAST.JValue
import org.json4s.DefaultFormats
import org.json4s.jvalue2extractable
import org.json4s.jvalue2monadic

case class Date(yearMonth: YearMonth, idx: Long)

case class Result(date: Date, actual: Int, expected: Double, threshold: Double, trend: Int, exceed: Double, weights: IndexedSeq[Double]){
  lazy val isAlert = actual > threshold
}
case object Result {
  implicit val formats = DefaultFormats

  def apply(date: Date, actual: Int, json: JValue): Result = Result(
      date,     
      actual,
      (json \ "expected").extract[Double],
      (json \ "threshold").extract[Double],
      (json \ "trend").extract[Int],    
      (json \ "exceed").extract[Double],
      (json \ "weights").extract[List[Double]].toIndexedSeq
    )
}