package sampler.spike.spatial

import sampler.data.DistributionBuilder
import sampler.math.Random
import java.nio.file.Files
import java.nio.file.Paths
import sampler.r.script.RScript

object PartitionApp extends App {
	import Helpers._
	implicit val r = Random
	val fileOut = Paths.get("results", "points.csv")
	
	def randomCoord = DistributionBuilder.uniform(0, 100).map(_*10.toInt/10.0).until(_.size == 2).map(s => (s(0), s(1)))
	val points = (1 to 1000).map{i => randomCoord.sample}.toSet
	
	//points.foreach(println)
	val lines = Partition(points)(RandomAxis.better).toSeq.zipWithIndex.flatMap{case (set, idx) =>
		set.map(pt => s"$idx, ${pt._1}, ${pt._2}")
	}
	println("==== DONE ====")
	
	val writer = Files.newBufferedWriter(fileOut)
	writer.append("Id, X, Y"); writer.newLine
	lines.foreach { line =>
		writer.append(line); writer.newLine
		println(line)
	}
	writer.close
	
	val rScript = s"""
		lapply(c("ggplot2", "reshape"), require, character.only=T) 
		data = read.csv("${fileOut.toAbsolutePath()}", colClasses=c('factor', 'numeric', 'numeric'))
		pdf("partition.pdf", width=4.13, height=2.91)
		ggplot(data, aes(x=X,y=Y, colour=Id)) + geom_point()
		dev.off()
	"""
	
	RScript.apply(rScript, fileOut.getParent.resolve("script.r"))
}

object Helpers{
	type Group[T] = Set[(T,T)]
	implicit val r = Random
	
	trait Splitter[T] {
		def apply(items: Group[T]): Option[Set[Group[T]]]
	}
	
	object RandomAxis {
		def apply[T: Fractional](items: Group[T]): Option[Set[Group[T]]] = {
			val n =  implicitly[Fractional[T]]
			import n._
			val splitCoordinate = 
				if(DistributionBuilder.bernoulli(0.5).sample) (p:(T,T)) => p._1 
				else (p:(T,T)) => p._2
			val sum = items.foldLeft(n.zero){case (acc, i) => n.plus(acc, splitCoordinate(i))}
			val meanCoord = n.div(sum, n.fromInt(items.size))
			val (s1, s2) = items.partition(p => n.lt(splitCoordinate(p), meanCoord))
			val result = if(s1.size >= 5 & s2.size >= 5) Some(Set(s1,s2))
			else None
			println(s"split $items")
			println(s"  res $result")
			result
		}
		
		def better(items: Group[Double]): Option[Set[Group[Double]]] = {
			val x = (p: (Double, Double)) => p._1
			val y = (p: (Double, Double)) => p._2
			
			def getExtent(coord: ((Double, Double)) => Double) = {
				val values = items.map(p => coord(p))
				values.max - values.min
			}
			
			def splitBy(coord: ((Double, Double)) => Double) = {
				val sum = items.foldLeft(0.0){case (acc, i) => acc + coord(i)}
				val meanCoord = sum / items.size
				items.partition(p => coord(p) < meanCoord)
			}
			
			val (s1, s2) = 
				if(getExtent(x) < getExtent(y)){
					splitBy(y)
				} else {
					splitBy(x)
				}
			val result = if(s1.size >= 5 & s2.size >= 5) Some(Set(s1,s2))
			else None
			
			result
		}
	}
	
	object Partition {	
		def apply[T](group: Group[T])(splitter: Group[T] => Option[Set[Group[T]]]): Set[Group[T]] = {
			
			splitter(group).map{subgroups =>
				subgroups.flatMap{subgroup => 
					apply(subgroup)(splitter)
				}
			}.getOrElse{
				println("  fallback "+group)
				Set(group)
			}
			
			//todo second tryif split fails?
			//todo write as for comprehension
			//todo tail recursion
		}
	}
}