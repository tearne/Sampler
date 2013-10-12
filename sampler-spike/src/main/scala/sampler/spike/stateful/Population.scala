package sampler.spike.stateful

import scalaz._
import Scalaz._
import sampler.spike.TablePopulation
import sampler.math.Random

object StatefulSamplable{
	type Counts[T] = Map[T, Int]
	
	def empty[T]: State[Counts[T], Counts[T]] = state(Map[T, Int]())
	
	/*
	 * Remove a single random item
	 */
	def drawOne[T](soFar: Counts[T]): State[Counts[T], Counts[T]] = for(
		item <- State[Counts[T], T]{s =>
			val (colours, counts) = s.toIndexedSeq.unzip
			val ballIndex = (math.random * s.values.sum).toInt
			val selected = colours(
				counts.tail
					.view
					.scanLeft(counts(0))(_ + _)
					.indexWhere(_ >= ballIndex)
			)
			(s.updated(selected, s(selected) - 1), selected)
		}
	) yield soFar.updated(item, soFar.getOrElse(item, 0) + 1)
	
	/*
	 * Remove a number of items
	 */
	def draw[T](n: Int = 1): State[Counts[T], Counts[T]] = for(
		balls <- get[Counts[T]];
		selection <- {
			assert(balls.values.sum >= n)	//Check the bag has enough left
			(1 to n).foldLeft(empty[T])((accState, _) => accState.flatMap(drawOne))
		}
	) yield selection
	
	/*
	 * Increase the number of items to select from to bring the total up to the
	 * requirement, returning the number of items added.
	 */
	def topUp[T](requirement: Counts[T]): State[Counts[T], Counts[T]] = for(
		current <- get[Counts[T]];
		added <- State{current: Counts[T] =>
			val toAdd = current.map{case (item, count) => item -> math.max(requirement(item) - count, 0)}
			val augmented = current.map{case (item, count) => item -> (count + toAdd(item))}
			(augmented, toAdd)
		}
	) yield added
}

object Test extends App{
	import StatefulSamplable._
	
	trait Colour
	object Red extends Colour { override def toString = "Red"}
	object Green extends Colour { override def toString = "Green"}
	object Blue extends Colour { override def toString = "Blue"}
	
	implicit val r = Random
	val reps = 10000000
	val startBag = Map(Red -> 300, Green -> 200, Blue -> 100)
	val n = 1
	
	
	var start = System.currentTimeMillis()
	
	(1 to reps).foreach{i => 
		val population = TablePopulation(startBag)
		val (remaining, selected) = population.remove(n)
	}
	
	var end = System.currentTimeMillis()
		
	println(f"Core scala approach took ${(end-start)/1000.0}%2.2f")
	
	
	start = System.currentTimeMillis()
	
	(1 to reps).foreach{i => 
		val full = startBag
		val (remaining, selected) = draw(n)(full)
	}
	
	end = System.currentTimeMillis()
		
	println(f"State monad approach took ${(end-start)/1000.0}%2.2f")
	
	
	
	
	
	
	

	
	
	
	
	
//	println(" Selected: "+selected)
//	println("Remaining: "+remaining)
}
