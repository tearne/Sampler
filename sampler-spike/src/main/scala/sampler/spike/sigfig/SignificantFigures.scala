package sampler.spike.sigfig

import java.math.RoundingMode

object SignificantFigures extends App{

	val numberIt = Stream.continually(math.random).iterator
	
	val numbers = numberIt.take(5).toList
	
	val methodA = numbers
		.map{d => BigDecimal(d).setScale(3, BigDecimal.RoundingMode.HALF_UP).doubleValue.toString}
	
	val methodB = numbers.map(d => f"$d%.3f")
	
//	(numbers, methodA, methodB).zipped.toList.foreach{case (n,a,b) => println(s"$a, $b, $n")}
	
	val lotsNumbers = numberIt.take(50000)
	
	def timeMe(times: Int, f : => String) = {
		val start = System.currentTimeMillis

		(1 to times).foreach(_ => f)
		
		val stop = System.currentTimeMillis
		stop - start
	}
	
	println(timeMe(1000000, {
		val d = numberIt.next
				f"$d%.3f"
	}))

	println(timeMe(1000000, 
			BigDecimal(numberIt.next).setScale(3, BigDecimal.RoundingMode.HALF_UP).doubleValue.toString
	))
	
	
	
}