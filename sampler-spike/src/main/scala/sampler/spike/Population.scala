/*
 * Copyright (c) 2012 Crown Copyright 
 *                    Animal Health and Veterinary Laboratories Agency
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sampler.spike

import scala.annotation.tailrec
import sampler.math.Random

// TODO: Think about creating another object to top up (helper method in companion object)

/** Trait to create a representation of a population
 *  
 *  Implementations of this trait can determine how the members of the population
 *  are stored and accessed giving users the ability to choose the most appropriate
 *  structure for their specific task
 */
trait Population[T]{
  def remove(num: Int): (Population[T], Population[T])

    /** The number of individuals within the population 
     *  
     *  @return the size of the population
     *  */
  def size: Int
}

/** An implementation of Population where the population structure is stored
 *  in a map of individuals of a specific category to a count of the number of times
 *  those individuals are represented in the population
 *  
 *  @param counts Map of population members of type T to a count of the number of members in the population
 */
case class TablePopulation[T](counts: Map[T, Int])(implicit val r: Random) extends Population[T]{

  private def reMap(l: List[T]) = {
    l.distinct.map(a => (a -> l.count(_ == a))).toMap
  }
  
  /** Remove a number of individuals from the population, without replacement
   *  
   *  @param num The number of members to be removed
   *  @return A tuple to TablePopulation objects, the first containing the individuals sampled the second the remaining population
   */
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
  
  /** Adds a table population to the current one to produce a combined object
   *  
   *  @param pop The population to be added
   *  @return A combined population containing all the individuals from both the current object and those in pop
   */
  def +(pop: TablePopulation[T]) = {
    val thatMap = pop.toMap
    
    val theSet = counts.map(_._1).++(thatMap.map(_._1).toSet)
    
    val newMap = theSet.map(a => a -> (counts.getOrElse(a, 0) + thatMap.getOrElse(a, 0))).toMap
    
    new TablePopulation(newMap)
  }
  
  def size = counts.values.sum
  
  /** Returns the map of individual character traits to the count of that individual in the population
   *  
   *  @return Map of member type to count
   */
  def toMap = counts
}

/** An implementation of Population where the population structure is stored
 *  as a sequence of each individual member of that population
 *  
 *  @param individuals A sequence containing all the members of the population
 */
case class SetPopulation[T](individuals: IndexedSeq[T])(implicit val r: Random) extends Population[T]{
  
  /** Remove a number of individuals from the population, without replacement
   *  
   *  @param num The number of members to be removed
   *  @return A tuple to TablePopulation objects, the first containing the individuals sampled the second the remaining population
   */
  def remove(num: Int = 1) = {

    assert(num <= size && num > 0, s"cannot sample ${num} elements")
    
	@tailrec
    def takeAnother(acc: IndexedSeq[T], bag: IndexedSeq[T]): IndexedSeq[T] = {
	  if(acc.size == num) acc
	  else{ 
		val item = bag(r.nextInt(bag.size))
		takeAnother(item +: acc, bag diff IndexedSeq(item))
	  }
	}
	
	val selected = takeAnother(IndexedSeq(), individuals)
	val remaining = (individuals diff selected)
	
	(SetPopulation(selected), SetPopulation(remaining))
  }
  
  /** Adds a set population to the current one to produce a combined object
   *  
   *  @param pop The population to be added
   *  @return A combined population containing all the individuals from both the current object and those in pop
   */
  def +(pop: SetPopulation[T]) = {
    new SetPopulation(individuals ++ pop.values)
  }
  
  def size: Int = individuals.size
  
  /** Returns a set containing all the members of the population
   *  
   *  @return set containing the whole population
   */
  def values = individuals
}