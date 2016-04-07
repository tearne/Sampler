package farrington.core.result

import org.json4s.DefaultFormats
import org.json4s.JsonAST.JValue

case class ResultVector(date: IndexedSeq[Date], actual: IndexedSeq[Int], expected: IndexedSeq[Double], threshold: IndexedSeq[Double], trend: IndexedSeq[Int], exceed: IndexedSeq[Double], weights: IndexedSeq[Double]){
  lazy val isAlert = (0 until threshold.length).map(i => (actual(i) > threshold(i)))
}
object ResultVector{
  implicit val formats = DefaultFormats

  def apply(date: IndexedSeq[Date], actual: IndexedSeq[Int], json: JValue): ResultVector = ResultVector(
      date,     
      actual,
      (json \ "expected").extract[List[Double]].toIndexedSeq,
      (json \ "threshold").extract[List[Double]].toIndexedSeq,
      (json \ "trend").extract[List[Int]].toIndexedSeq,    
      (json \ "exceed").extract[List[Double]].toIndexedSeq,
      (json \ "weights").extract[List[Double]].toIndexedSeq
    )
}