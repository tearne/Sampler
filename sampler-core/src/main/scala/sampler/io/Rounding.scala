package sampler.io

import BigDecimal.RoundingMode

trait Rounding {
	//TODO untested
  //TODO this with typeclasses?
	
	implicit class Roundable(d: Double){
		def decimalPlaces(n: Int) = BigDecimal(d).setScale(n, RoundingMode.HALF_UP).doubleValue
		def significanatFigures(n: Int) = {
			val bd = BigDecimal(d)
			val newScale = n - bd.precision + bd.scale
			bd.setScale(newScale, RoundingMode.HALF_UP).doubleValue
		}
	}
}

object Rounding extends Rounding