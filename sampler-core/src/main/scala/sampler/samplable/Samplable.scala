package sampler.samplable

import sampler.math.Random
import scala.language.higherKinds
import cats.Monad
import scala.annotation.tailrec
import scala.IndexedSeq

/*
 *  Typeclass for 'things which can be sampled from'
 *  Note it's a Monad, so must also have pure & flatMap
 *  
 *  From Applicative
 *  def pure[A](x: A): F[A]
 *  
 *  From FlatMap
 *  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
 *  
 *  From Monad
 *  override def map[A, B](fa: F[A])(f: A => B): F[B] =
 *    flatMap(fa)(a => pure(f(a)))
 *  
 */
trait Samplable[T[_]] extends Monad[T]{
  // Draw a value
  def sample[A](ta: T[A])(implicit r: Random): A
  
  // Build a new samplable using some transformation f of this samplable
  def from[A](f: Random => A): T[A]
  
  def filter[A](ta: T[A])(predicate: A => Boolean): T[A] = from{ r =>
    @tailrec
		def tryAgain: A = {
			val s = sample(ta)(r)
			if(predicate(s)) s
			else tryAgain
		}
		tryAgain
  }
  
  def until[A](ta: T[A])(predicate: IndexedSeq[A] => Boolean): T[IndexedSeq[A]] = from{ r => 
    @tailrec
		def append(previous: IndexedSeq[A]): IndexedSeq[A] = {
			if(predicate(previous)) previous
			else append(previous.:+(sample(ta)(r)))
		}
		append(IndexedSeq( sample(ta)(r) ))
  }
  
  def combine[A,B, C](ta: T[A], tb: T[B])(f: (A, B) => C): T[C] = from{r =>
    f(sample(ta)(r), sample(tb)(r))  
  }
}

object Samplable{
  def apply[F[_]](implicit s: Samplable[F]): Samplable[F] = s
}