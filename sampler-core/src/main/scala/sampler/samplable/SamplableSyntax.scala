package sampler.samplable

import scala.language.higherKinds

trait SamplableSyntax{
  /*
   * Type enrichment for Samplables
   * Note that you also get the operations defined on the Samplable
   * type class, e.g. flatMap & map via the Samplable companion
   * object
   */
  implicit class SamplableOps[D[_], A](thiz: D[A]){
    def flatMap[B](f: A => D[B])(implicit instance: Samplable[D]) = 
      instance.flatMap(thiz)(f)

    def map[B](f: A => B)(implicit instance: Samplable[D]) =
      instance.map(thiz)(f)
    
    def filter(predicate: A => Boolean)(implicit instance: Samplable[D]) = 
      instance.filter(thiz)(predicate)

    def until(predicate: IndexedSeq[A] => Boolean)(implicit instance: Samplable[D]) =
      instance.until(thiz)(predicate)
      
    def combine[B,C](that: D[B])(op: (A,B) => C)(implicit  instance: Samplable[D]) =
      instance.combine(thiz, that)(op)

    def convolve(that: D[A])(implicit instance: Samplable[D], num: Numeric[A]) = 
      instance.combine(thiz, that)(num.plus)

    def crossCorrelate(that: D[A])(implicit instance: Samplable[D], num: Numeric[A]) = 
      instance.combine(thiz, that)(num.minus)
  }
}