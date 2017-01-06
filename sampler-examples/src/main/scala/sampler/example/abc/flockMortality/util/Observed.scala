package sampler.example.abc.flockMortality.util

import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Writes

case class Observed(
    id: Int,
    flockSize: Int,
    days: IndexedSeq[Int],
    eggs: IndexedSeq[Int],
    dead: IndexedSeq[Int],
    infectionFreeDays: Int
)
{
  // Find day of first significant death
  lazy val deadStart: Int = {
    val level = 0.001 * flockSize
    (dead zip days).filter(d => d._1 >= level).head._2
  }
  
  // Find day of peak in mortality
  lazy val deadPeak: Int = (dead zip days).filter(d => d._1 == dead.max).head._2
  
  // Find index of first egg production data point 
  lazy val firstEggs: Int = eggs.zipWithIndex.filter(e => e._1 >= 0).head._2
  
  // Find index of lowest egg production data
  lazy val eggsMin: Int = (eggs zip days).filter(e => e._1 == eggs.min).head._2
  
  // Mean eggs produced prior to infection and egg coefficient (mean eggs per bird)
  lazy val meanEggs = 
    if (infectionFreeDays <= firstEggs) eggs(firstEggs)
    else {  
      val validEggs = eggs.take(infectionFreeDays).filter{j => j >= 0}
      validEggs.sum.toDouble / validEggs.length
    }
  
  // Egg coefficient (mean eggs per bird)
  lazy val eggCoeff = meanEggs / flockSize  
}

case object Observed {
  def apply(json: JsValue): IndexedSeq[Observed] = {
    val json_sheds = (json \ "observed").as[List[JsValue]]
    json_sheds.map{ shed => Observed(
      (shed \ "id").as[Int],
      (shed \ "size").as[Int],
      (shed \ "days").as[List[Int]].toIndexedSeq,
      (shed \ "eggs").as[List[Int]].toIndexedSeq,
      (shed \ "dead").as[List[Int]].toIndexedSeq,
      (shed \ "infectionFreeDays").as[Int] )
    }.toIndexedSeq
  }
  
  implicit val observedWrites = new Writes[Observed] {
    def writes(data: Observed) = { Json.obj(
          "id" -> data.id,
          "size" -> data.flockSize,
          "days" -> data.days,
          "eggs" -> data.eggs,
          "dead" -> data.dead,
          "infectionFreeDays" -> data.infectionFreeDays
      )
    }
  }
  
  def toDaily(obs: Observed): Observed = {
    import obs._
    val dailyDead = (0 until dead.size).map{ i => 
      if (i == 0) dead(i)
      else dead(i) - dead(i-1)
    }
    Observed(id, flockSize, days, eggs, dailyDead, infectionFreeDays)
  }
  
}