package sampler.spike.distribution.tearne

import cats.Monad
import scala.language.higherKinds
import sampler.math.{Random, Partition, AliasTable}
import scala.annotation.tailrec
import scala.collection.{GenSeq, GenMap}

//TODO Applicative

sealed trait Distribution[A] {
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

final case class Combine[A,B,C] (
    dA: Distribution[A], 
    dB: Distribution[B], 
    f: (A,B) => C
    ) extends Distribution[C] {
  def sample(implicit r: Random) = f(dA.sample, dB.sample)
}

final case class EmpiricalTable[A](weightsByItem: Map[A, Double]) extends Distribution[A] {
  val (items, weights) = weightsByItem.toIndexedSeq.unzip
  assume(weights.find(_ <= 0).isEmpty, "Found negative weights.")
  val probPartition = Partition.fromWeights(weights)
  val aliasTable = new AliasTable(probPartition)
  def sample(implicit r: Random) = items(aliasTable.next(r))
}

final case class EmpiricalSeq[A](items: IndexedSeq[A]) extends Distribution[A] {
  val size = items.size
  def sample(implicit r: Random) = items(r.nextInt(size))
}

final case class Function[A](f: Random => A) extends Distribution[A] {
   def sample(implicit r: Random) = f(r)
}

object Distribution extends LowPriorityImplicits {
  def fromTable[A](weightsByItem: Map[A, Double]) = 
    EmpiricalTable(weightsByItem: Map[A, Double])
  
  def fromSequence[A](items: IndexedSeq[A]) = 
    EmpiricalSeq(items)
  
  def fromFunction[A](f: Random => A) = 
    Function(f)
  
  def exponential(rate: Double) = fromFunction{(r: Random) => 
    - math.log(r.nextDouble) / rate
  }
}

trait LowPriorityImplicits {
  implicit val instances = new Samplable[Distribution] with Monad[Distribution] {
    override def pure[A](a: A): Distribution[A] = Pure(a)
    override def flatMap[A,B](e: Distribution[A])(f: A => Distribution[B]): Distribution[B] = e.flatMap(f)
    override def sample[A](e: Distribution[A])(implicit r: Random) = e.sample
  }
}