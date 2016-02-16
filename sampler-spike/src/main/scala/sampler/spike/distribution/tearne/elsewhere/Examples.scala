package sampler.spike.distribution.tearne.elsewhere

import sampler.math.Random
import sampler.spike.distribution.tearne._

object Exponential extends App {
  //Exponential of rate 3
  val rate = 3.0
  def krazyFunktion(r: Random) = - math.log(r.nextDouble) / rate
  val expDist = (krazyFunktion _).distribution
  
  val samples = 1000000
  implicit val r = Random
  val mean = Stream.continually(expDist.sample).take(samples).sum / samples
  println(s"mean ~= $mean")
}

object AnimalTable extends App {
  val dist: Distribution[String] = Map(
    "Pig" -> 10.0,
    "Duck" -> 20.0,
    "Cow" -> 30.0
  ).distribution
  
  val threePigDist = dist.until(
    _.groupBy(identity)
      .get("Pig")
      .map(_.size == 3)
      .getOrElse(false)
    )
  
  println("If you want 3 pigs...")
  threePigDist.sample(Random)
    .groupBy(identity)
    .mapValues(_.size)
    .foreach(println)
}

object ChocolateSequence extends App {
  val chocolates = Seq("Milk", "Dark", "Dark", "Milk", "Milk")
  
  implicit val r: Random = Random
  val fourMilkDist = chocolates
    .distribution
    .until(soFar =>
      soFar.size > 4 &&
      !soFar.takeRight(4).exists(_ != "Milk")
    )
    
  fourMilkDist
    .until(_.size == 10)
    .sample
    .foreach(println)
}

object Expiress extends App {
  val starter: Distribution[String] = Seq(
      "Garlic bread", 
      "Dough balls", 
      "Bruschetta")
    .distribution
    
  val main: Distribution[String] = Seq(
      "Rustichella",
      "American",
      "Padana")
    .distribution
  
  val pud: Distribution[String] = Seq(
      "Fudge cake",
      "Ice cream",
      "Hot chocolate")
    .distribution
    
  implicit val r = Random
  
  val res = Samplable[Distribution].map3(
    starter,
    main,
    pud
  )("I'd like "+_ +", "+ _ +" and "+ _)
    .until(_.size == 3)
    .sample
    
  res.foreach(println)

  //TODO tie fighter not working
//  import cats._
//  import cats.syntax.all._
//  import cats.syntax.cartesian._
//  (starter |@| mainCourse |@| pud).map(_ + _ + _)
//    .sample
//    .foreach(println)
}