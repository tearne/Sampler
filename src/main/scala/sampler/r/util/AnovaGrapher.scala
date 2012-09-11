package sampler.r.util
import sampler.r.wrap.AnovaResults

class AnovaGrapher {

	def apply(results: AnovaResults) = {
		
		var min = 10.0
		
		results.paramEntries.map {
			case a if(a.fValue < min) => min = a.fValue
		}
		
		results.paramEntries.map {
			case a => {
				print(a.name + ": ")
				val numStars = (a.fValue/min).asInstanceOf[Int]
				for(i <- 0 until numStars)
					print("*")
				print("\t" + a.fValue)
				print("\n")
			}
		}
	}
	
}