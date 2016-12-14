package sampler.maths

trait RangeCheckImplicits {
  implicit class NumberOps[A: Fractional](num: A){
    def isProbability = {
      val f = implicitly[Fractional[A]]
      f.gteq(num, f.zero) && f.lteq(num, f.one)
    }
  }
  
  implicit class TraversableOps[A: Fractional](numbers: Traversable[A]){
    def areProbabilities = {
      !numbers.exists(!_.isProbability)
    }
  }
}
