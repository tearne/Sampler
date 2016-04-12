package sampler.spike.sigfig

import java.math.RoundingMode
import java.math.MathContext

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
	
	println("""f"${1.23456789}%.4f" = """ + f"${1.23456789}%.4f")
	println(timeMe(1000000, {
		val d = numberIt.next
				f"$d%.4f"
	})+" for String formatting")

	println()
	println("""BigDecimal("1.23456789").setScale(4, BigDecimal.RoundingMode.HALF_UP).doubleValue = """+BigDecimal("1.23456789").setScale(4, BigDecimal.RoundingMode.HALF_UP).doubleValue)
	println(timeMe(1000000, 
			BigDecimal(numberIt.next).setScale(4, BigDecimal.RoundingMode.HALF_UP).doubleValue.toString
	)+" for BigDecimal with Scale")
	
	println()
	val mc = new MathContext(4)
	println("""BigDecimal("1.23456789", mc).doubleValue = """+BigDecimal("1.23456789", mc).doubleValue)
	println(timeMe(1000000, 
			BigDecimal(numberIt.next, mc).doubleValue.toString
	)+" for BigDecimal with MathContext")
	
	
	
}