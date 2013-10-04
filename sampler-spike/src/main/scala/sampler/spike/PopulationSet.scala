//package sampler.spike
//
//import scala.annotation.tailrec
//import sampler.math.Random
//
//class Population[T](pop: Map[T, Int])(implicit r: Random) {
//	
//  private def expand(s: T, i: Int) = (1 to i).map(_ => s)
//  
//  private lazy val flatPop = pop.flatMap(x => expand(x._1,x._2)).toIndexedSeq
//  
//  private lazy val popSize = flatPop.size
//  
//  private def checkSize(size: Int) = {
//	  assert(size>0, "Cannot sample 0 or less objects")
//	  assert(size<=popSize, "Cannot sample more elements than there are in the population")
//  }
//  
//  private def reMap(l: List[T]) = {
//    l.distinct.map(a => (a -> l.count(_ == a))).toMap
//  }
//  
//  def sampleWithoutReplacement(sampleSize: Int) = {
//    checkSize(sampleSize)
//    
//	@tailrec
//    def takeAnother(acc: List[T], bag: IndexedSeq[T]): List[T] = {
//	  if(acc.size == sampleSize) acc
//	  else{ 
//		val item = bag(r.nextInt(bag.size))
//		takeAnother(item +: acc, bag diff List(item))
//	  }
//	}
//	
//	val selected = takeAnother(Nil, flatPop)
//	val remaining = (flatPop diff selected).toList
//	
//	(reMap(selected), reMap(remaining))
//  }
//  
//  def sampleWithReplacement(sampleSize: Int) = {
//    checkSize(sampleSize) //TODO
//    
//    reMap((1 to sampleSize).map(_ => flatPop(r.nextInt(popSize))).toList)
//  }
//}