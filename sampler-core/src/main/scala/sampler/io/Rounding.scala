package sampler.io

trait Rounding {
	//TODO untested
	
	implicit class Roundable(d: Double){
		def decimalPlaces(n: Int) = BigDecimal(d).setScale(n, BigDecimal.RoundingMode.HALF_UP).doubleValue
	}
}