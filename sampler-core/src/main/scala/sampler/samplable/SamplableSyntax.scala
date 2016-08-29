package sampler.samplable

import sampler.maths.Random

import scala.language.higherKinds

trait SamplableSyntax{
  /*
   * Type enrichment for Samplables
   * Note that you also get the operations defined on the Samplable
   * type class, e.g. flatMap & map via the Samplable companion
   * object
   */
  implicit class SamplableOps[T[_], A](thiz: T[A]){
    def sample[A](implicit instance: Samplable[T], r: Random) =
      instance.sample(thiz)(r)

    def sample[A](r: Random)(implicit instance: Samplable[T]) =
      instance.sample(thiz)(r)

    def flatMap[B](f: A => T[B])(implicit instance: Samplable[T]) = 
      instance.flatMap(thiz)(f)

    def map[B](f: A => B)(implicit instance: Samplable[T]) =
      instance.map(thiz)(f)
    
    def filter(predicate: A => Boolean)(implicit instance: Samplable[T]) = 
      instance.filter(thiz)(predicate)

    def until(predicate: IndexedSeq[A] => Boolean)(implicit instance: Samplable[T]) =
      instance.until(thiz)(predicate)
      
    def combine[B,C](that: T[B])(op: (A,B) => C)(implicit  instance: Samplable[T]) =
      instance.combine(thiz, that)(op)

    def convolve(that: T[A])(implicit instance: Samplable[T], num: Numeric[A]) = 
      instance.combine(thiz, that)(num.plus)

    def crossCorrelate(that: T[A])(implicit instance: Samplable[T], num: Numeric[A]) = 
      instance.combine(thiz, that)(num.minus)
  }
}