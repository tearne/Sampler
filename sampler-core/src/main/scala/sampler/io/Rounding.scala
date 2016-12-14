package sampler.io

trait Rounding {
	//TODO untested
  //TODO this with typeclasses?
	
	implicit class Roundable(d: Double){
		def decimalPlaces(n: Int) = BigDecimal(d).setScale(n, BigDecimal.RoundingMode.HALF_UP).doubleValue
	}
}

object Rounding extends Rounding