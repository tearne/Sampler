package sampler.example.abc.flockMortality.util

import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Writes

case class FittedResult(
    id: Int,
    size: Int,
    days: IndexedSeq[Int],
    obsEggs: IndexedSeq[Int],
    obsDead: IndexedSeq[Int],
    fitEggs: IndexedSeq[Double],
    fitDead: IndexedSeq[Double],
    params: Parameters
)
object FittedResult {
  
  def apply(simulated: SimulatedResult): IndexedSeq[FittedResult] = {  
    
    // Converts list of integers to list of their cumulative sum
    def toCumulative(daily: IndexedSeq[Int]): IndexedSeq[Int] = {
      daily.scanLeft(0){case (a,v)=> a + v}.tail
    }
    
    // Converts list of cumulative sum to their daily values
    def toDaily(cumulative:  IndexedSeq[Double]):  IndexedSeq[Double] = {
      val daily = (0 until cumulative.size).map{ i => 
        if (i == 0) cumulative(i)
        else cumulative(i) - cumulative(i-1)
      }
      daily
    }
    
    import simulated._
    val orderedStates = dayStates.map(map => map.toSeq.sortBy(_._1))
    val simMap = orderedStates.zip(observed)
    
    // Plot daily mortality
    simMap.map{ i => FittedResult(
      i._2.id,
      i._2.flockSize,
      i._2.days,
      i._2.eggs,
      i._2.dead,
      i._1.map(state => state._2.eggs).toIndexedSeq,
      toDaily(i._1.map(state => state._2.d).toIndexedSeq),
      params)
    }.toIndexedSeq
    
  }
  
  // Function to write Fitted object as json
  implicit val fittedWrites = new Writes[FittedResult] {
    def writes(fit: FittedResult) = {  
      
      // Truncates doubles
      def truncate(d: Double, n: Int = 6) = {
        BigDecimal(d).setScale(n, BigDecimal.RoundingMode.HALF_UP).toDouble
      }
      import fit._
      Json.obj(
        "properties" -> Json.obj(
          ("id" -> id),
          ("size" -> size),
          ("days" -> days)
        ),
        "observed" -> Json.obj(
          ("eggs" -> obsEggs),
          ("dead" -> obsDead)
        ),
        "fitted" -> Json.obj(
          ("eggs" -> fitEggs.map(i => truncate(i))),
          ("dead" -> fitDead.map(i => truncate(i)))
        ),
        "median" -> Json.obj(
            ("beta" -> params.beta),
            ("eta" -> params.eta),
            ("gamma" -> params.gamma),
            ("delta" -> params.delta),
            ("sigma" -> params.sigma),
            ("sigma2" -> params.sigma2),
            ("offset" -> params.offset)
        )
      )
    }
  }
  
  
}