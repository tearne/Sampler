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
//  def +(pop: Population[T])
  def size: Int
}

// T: Spotted (black, white)
case class TablePopulation[T](counts: Map[T, Int])(implicit val r: Random) extends Population[T]{
  //...
  private def reMap(l: List[T]) = {
    l.distinct.map(a => (a -> l.count(_ == a))).toMap
  }
  
  def remove(num: Int = 1) = {
    
    assert(num <= size && num > 0, s"cannot sample ${num} elements")
    
    def sampleOne(currentPop: List[(Int, T)]) = {
      def findIndex(list: List[Int], index: Int, selection: Int): Int	= {
        val runningSum = list.take(index+1).sum
        if(selection <= runningSum) index
    	else findIndex(list, index+1, selection)
      }
      
      val counts = currentPop.map(_._1)
    	
      val selectedIndex = findIndex(counts, 0, r.nextInt(counts.sum))
    				  
      val selection = currentPop(selectedIndex)._2
    				  
      val toPatch = {
        val newCount = currentPop(selectedIndex)._1 - 1
    	if(newCount == 0) List()
    	else List((newCount, currentPop(selectedIndex)._2))
      }
    	
      val newCounts = currentPop.patch(selectedIndex, toPatch, 1)
      (selection, newCounts)
    }
      
    def buildSamples(l: List[T], interimPop: List[(Int, T)]): (List[T], List[(Int,T)]) = {
      if(l.size == num) (l, interimPop)
      else {
        val nextSample = sampleOne(interimPop)
        val samples = l.:+(nextSample._1)
        buildSamples(samples, nextSample._2)
      }
    }
    
    val reverse = counts.toList.map{case (a,b) => (b,a)}
    
    val result = buildSamples(List(), reverse)
      
    val selectedPop = TablePopulation(reMap(result._1))
    val remainingPop = TablePopulation(result._2.map{case (b,a) => (a,b)}.toMap)
      
    (selectedPop, remainingPop)
  }
  
  def +(pop: TablePopulation[T]) = {
    val thatMap = pop.toMap
    
    val theSet = counts.map(_._1).++(thatMap.map(_._1).toSet)
    
    val newMap = theSet.map(a => a -> (counts.getOrElse(a, 0) + thatMap.getOrElse(a, 0))).toMap
    
    new TablePopulation(newMap)
  }
  
  def size = counts.values.sum
  
  def toMap = counts
}

//T: Characteristics(mood: MoodEnum, age: Double, infected: Date))
case class SetPopulation[T](individuals: IndexedSeq[T])(implicit val r: Random) extends Population[T]{
  //...
  def remove(num: Int) = {

    assert(num <= size && num > 0, s"cannot sample ${num} elements")
    
	@tailrec
    def takeAnother(acc: IndexedSeq[T], bag: IndexedSeq[T]): IndexedSeq[T] = {
	  if(acc.size == num) acc
	  else{ 
		val item = bag(r.nextInt(bag.size))
		takeAnother(item +: acc, bag diff IndexedSeq(item))
	  }
	}
	
	val selected = takeAnother(IndexedSeq(), individuals.toIndexedSeq)
	val remaining = (individuals diff selected)
	
	(SetPopulation(selected), SetPopulation(remaining))
  }
  
  def +(pop: Population[T]) = ???
  
  def size: Int = individuals.size
  
  def values = individuals
}