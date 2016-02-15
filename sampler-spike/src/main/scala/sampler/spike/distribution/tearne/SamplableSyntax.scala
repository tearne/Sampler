package sampler.spike.distribution.tearne

import scala.language.higherKinds

trait SamplableSyntax{
  implicit class SamplableOps[D[_], A](thiz: D[A]){
    def filter(predicate: A => Boolean)(implicit instance: Samplable[D]) = {
      instance.filter(thiz)(predicate)
    }
    def until(predicate: IndexedSeq[A] => Boolean)(implicit instance: Samplable[D]) = {
      instance.until(thiz)(predicate)
    }
    def combine[B,C](that: D[B])(op: (A,B) => C)(implicit  instance: Samplable[D]) = {
      instance.combine(thiz, that)(op)
    }
    def convolve(that: D[A])(implicit instance: Samplable[D], num: Numeric[A]) = {
      instance.combine(thiz, that)(num.plus)
    }
    def crossCorrelate(that: D[A])(implicit instance: Samplable[D], num: Numeric[A]) = {
      instance.combine(thiz, that)(num.minus)
    }
  }
}