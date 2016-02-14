package sampler.spike.distribution.tearne.elsewhere

import sampler.math.Random
import sampler.spike.distribution.tearne._

object Elsewhere{
  val table: Map[String, Double] = Map(
    "Pig" -> 10,
    "Duck" -> 20,
    "Cow" -> 30
  )
  val observations = Seq("Milk", "Dark", "Dark", "Milk", "Milk")
  
  val chocDist = observations.empirical
  val animalDist = table.empirical
  
  implicit val r: Random = Random
  val fourMilkDist = chocDist
    .until(soFar =>
      soFar.size > 4 &&
      !soFar.takeRight(4).exists(_ != "Milk")
    )
    
  fourMilkDist
    .until(_.size == 10)
    .sample
    .foreach(println)
}