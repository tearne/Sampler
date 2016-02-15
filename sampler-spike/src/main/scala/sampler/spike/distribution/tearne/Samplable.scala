package sampler.spike.distribution.tearne

import sampler.math.Random
import scala.language.higherKinds
import cats.Monad
import scala.annotation.tailrec

trait Samplable[T[_]] extends Monad[T]{
  def sample[A](ta: T[A])(implicit r: Random): A
  def build[A](f: Random => A): T[A]
  
  def filter[A](ta: T[A])(predicate: A => Boolean): T[A] = build{ r =>
    @tailrec
		def tryAgain: A = {
			val s = sample(ta)(r)
			if(predicate(s)) s
			else tryAgain
		}
		tryAgain
  }
  
  def until[A](ta: T[A])(predicate: IndexedSeq[A] => Boolean): T[IndexedSeq[A]] = build{ r => 
    @tailrec
		def append(previous: IndexedSeq[A]): IndexedSeq[A] = {
			if(predicate(previous)) previous
			else append(previous.:+(sample(ta)(r)))
		}
		append(IndexedSeq( sample(ta)(r) ))
  }
  
  def combine[A,B, C](ta: T[A], tb: T[B])(f: (A, B) => C): T[C] = build{r =>
    f(sample(ta)(r), sample(tb)(r))  
  }
}

object Samplable{
  def apply[F[_]](implicit s: Samplable[F]): Samplable[F] = s
}