package sampler.spike.distribution.tearne

import sampler.math.Random
import scala.language.higherKinds

trait Samplable[F[_]]{
  def sample[A](fa: F[A])(implicit r: Random): A
}
object Samplable{
  def apply[F[_]](implicit s: Samplable[F]): Samplable[F] = s
}