package sampler.spike.distribution.tearne

import cats.Monad
import scala.language.higherKinds
import sampler.math.{Random, Partition, AliasTable}
import scala.annotation.tailrec
import scala.collection.{GenSeq, GenMap}

sealed trait Distribution[A]{
  def sample(implicit r: Random): A
  
  def flatMap[B](f: A => Distribution[B]) = FlatMap(this, f)
  def map[B](f: A => B) = FlatMap(this, (a: A) => Pure(f(a)))
  def until(predicate: IndexedSeq[A] => Boolean) = Until(this, predicate)
  def filter(predicate: A => Boolean) = Filter(this, predicate)
  def combine[B, C](that: Distribution[B])(op: (A, B) => C) = Combine(this, that, op)
}

final case class Pure[A](value: A) extends Distribution[A] {
  def sample(implicit r: Random) = value
}

final case class FlatMap[A,B](d: Distribution[A], f: A => Distribution[B]) extends Distribution[B]{
  def sample(implicit r: Random) = f(d.sample).sample
}

final case class Until[A](
    d: Distribution[A], 
    predicate: IndexedSeq[A] => Boolean
    ) extends Distribution[IndexedSeq[A]] {
  def sample(implicit r: Random) = {
		@tailrec
		def append(previous: IndexedSeq[A]): IndexedSeq[A] = {
			if(predicate(previous)) previous
			else append(previous.:+(d.sample))
		}
		append(IndexedSeq(d.sample))
	}
}

final case class Filter[A](
    d: Distribution[A], 
    predicate: A => Boolean
    ) extends Distribution[A] {
  def sample(implicit r: Random) = {
		@tailrec
		def tryAgain: A = {
			val s = d.sample
			if(predicate(s)) s
			else tryAgain
		}
		
		tryAgain
	}
}

final case class Combine[A,B,C](
    dA: Distribution[A], 
    dB: Distribution[B], 
    f: (A,B) => C
    ) extends Distribution[C] {
  def sample(implicit r: Random) = f(dA.sample, dB.sample)
}

final case class TableDist[A](weightsByItem: Map[A, Double]) extends Distribution[A]{
  val (items, weights) = weightsByItem.toIndexedSeq.unzip
  assume(weights.find(_ <= 0).isEmpty, "Found negative weights when building distribution.")
  val probPartition = Partition.fromWeights(weights)
  val aliasTable = new AliasTable(probPartition)
  def sample(implicit r: Random) = items(aliasTable.next(r))
}

final case class SeqDist[A](items: IndexedSeq[A]) extends Distribution[A]{
  val size = items.size
  def sample(implicit r: Random) = items(r.nextInt(size))
}

object Distribution {
  //TODO monad instance

  object ImplicitClasses {
    implicit class SeqOps[A](genSeq: GenSeq[A]) {
  		val indSeq = genSeq.toIndexedSeq
  		def toSeqDist = SeqDist[A](indSeq)
  		def toTableDist = TableDist[A](
  			indSeq.groupBy(identity).map{case (k,v) => k -> v.size.toDouble}
  		)
  	}
  	
  	implicit class MapOps[A](table: GenMap[A,Double]) {
  		def toTableDist = TableDist[A](table.seq.toMap)
  	}
  }  
}

object Discussion extends App {
  val table: Map[String, Double] = Map(
    "Pig" -> 10,
    "Duck" -> 20,
    "Cow" -> 30
  )
  val observations = Seq("Milk", "Dark", "Dark", "Milk", "Milk")
  
  import Distribution.ImplicitClasses._
  val chocDist: Distribution[String] = observations.toSeqDist
  val animalDist: Distribution[String] = table.toTableDist
  
  implicit val r = Random
  val fourMilkDist = chocDist
    .until(soFar =>
      soFar.size > 4 &&
      !soFar.takeRight(4).exists(_ != "Milk")
    )
    
  fourMilkDist
    .until(_.size == 10)
    .sample
    .foreach(println)
}