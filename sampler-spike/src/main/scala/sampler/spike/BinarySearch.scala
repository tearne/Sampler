package sampler.spike

import scala.annotation.tailrec
import scala.util.Sorting

object BinarySearch {
	def firstPassing[T: Ordering](monotoneMapping: T => Boolean)(items: Seq[T]): Option[T] = {
		if(items.size == 0) None
		else{
			val sorted = items.sorted
			@tailrec
			def go(min: Int, max: Int): T = {
				if(max == min) sorted(max)
				else{
					val midPoint = (max + min) / 2
					println("Trying "+midPoint)
					if(monotoneMapping(sorted(midPoint))) go(min, midPoint)
					else go(midPoint + 1, max)
				}
			}
			
			Some(go(0, items.size - 1))
		}
	}
}

//TODO make this a proper test
object BinaryTest extends App{
	val items = IndexedSeq(1,33,99,4095,120,133,147)
	val condition = (i: Int) => i%2 == 0
	println(BinarySearch.firstPassing(condition)(items))
}