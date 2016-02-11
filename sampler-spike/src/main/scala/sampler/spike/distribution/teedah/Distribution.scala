package sampler.spike.distribution.teedah

import cats.Monad
import scala.language.higherKinds

trait Distribution[D[_]] extends Monad[D]{
  def fromTable[A](weights: Map[A, Double]): D[A]
}