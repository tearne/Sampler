package sampler.spike

import scala.annotation.tailrec
import sampler.math.Random

/*
 * There are two kinds of population
 * 1) Tables of counts of similar entity
 * 2) Set of entity, which may all have different state
 * 
 * We want a way to move individuals or their representation
 * between populations
 */

// ANOTHER OBJECT TO TOP UP? (helper method in companion object)

trait Population[T]{
  def remove(num: Int): (Population[T], Population[T])
  def +(pop: Population[T]): Unit
  def size: Int
}

// T: Spotted (black, white)
case class TablePopulation[T](counts: Map[T, Int])(implicit val r: Random) extends Population[T]{
  //...
  private def reMap(l: List[T]) = {
    l.distinct.map(a => (a -> l.count(_ == a))).toMap
  }
  
  def remove(num: Int = 1) = {
    val reverse = counts.toList.map{case (a,b) => (b,a)}
    
    def doSampling(pop: List[(Int, T)]) = {
      
      def sampleOne(accSamples: List[T], currentPop: List[(Int, T)]) = {
    	def findIndex(list: List[Int], index: Int, selection: Int): Int	= {
    	  if(selection <= list(index)) index
    	  else findIndex(list, index+1, selection)
    	}
    		  
    	val selectedIndex = findIndex(reverse.map(_._1), 0, r.nextInt(size))
    				  
    	val selection = List(reverse(selectedIndex)._2)
    				  
    	val toPatch = {
    	  val newCount = reverse(selectedIndex)._1 - 1
    	  if(newCount == 0) List()
    	  else List((newCount, reverse(selectedIndex)._2))
    	}
    		  
    	val newCounts = reverse.patch(selectedIndex, toPatch, 1)
    				  
    	val newMap = (newCounts.map{case (b,a) => (a,b)}).toMap
    				  
    	(TablePopulation(reMap(selection)), TablePopulation(newMap))
      }
      
      sampleOne(List(), pop)
    }
    
    doSampling(reverse)
  }
  
  def +(pop: Population[T]) = ???
  
  def size = counts.values.sum
  
  def toMap = counts
}

//T: Characteristics(mood: MoodEnum, age: Double, infected: Date))
case class SetPopulation[T](individuals: Seq[T]) extends Population[T]{
  //...
  def remove(num: Int): (Population[T], Population[T]) = ???
  def +(pop: Population[T]): Unit = ???
  def size: Int = ???
}


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