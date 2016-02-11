package sampler.spike.distribution.tearne

import cats.Monad
import scala.language.higherKinds

trait Distribution[D[_]] extends Monad[D]{
  def fromTable[A](weights: Map[A, Double]): D[A]
}

object Discussion {
  val table: Map[Item, Double] = ???
  val observations: Seq[Items] = ???
  
  val d = observations.toDistirbubtion
  val samples = (1 to 100).map{_ => d.sample}
}