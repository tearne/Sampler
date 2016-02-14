package sampler.spike.distribution.tearne.elsewhere

import sampler.math.Random
import sampler.spike.distribution.tearne._

object Analytical extends App{
  val rate = 3.0
  def krazyFunktion(r: Random) = - math.log(r.nextDouble) / rate
  val expDist = (krazyFunktion _).distribution
  
  val samples = 1000000
  implicit val r = Random
  val mean = Stream.continually(expDist.sample).take(samples).sum / samples
  println("mean = "+mean)
}

object ChocolateSequence extends App {
  val chocDist = Seq("Milk", "Dark", "Dark", "Milk", "Milk").distribution
  
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

object AnimalTable extends App {
  val dist: Distribution[String] = Map(
    "Pig" -> 10.0,
    "Duck" -> 20.0,
    "Cow" -> 30.0
  ).distribution
  
  println("You were thinking of a "+dist.sample(Random))
}