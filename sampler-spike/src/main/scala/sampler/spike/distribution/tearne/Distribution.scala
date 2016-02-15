package sampler.spike.distribution.tearne

import cats.Monad
import scala.language.higherKinds
import sampler.math.{Random, Partition, AliasTable}
import scala.annotation.tailrec
import scala.collection.{GenSeq, GenMap}

//TODO Applicative

sealed trait Distribution[A] {
  def sample(implicit r: Random): A
}

final case class Build[A](f: Random => A) extends Distribution[A] {
  def sample(implicit r: Random) = f(r)
}

final case class Pure[A](value: A) extends Distribution[A] {
  def sample(implicit r: Random) = value
}

final case class FlatMap[A,B](d: Distribution[A], f: A => Distribution[B]) extends Distribution[B]{
  def sample(implicit r: Random) = f(d.sample).sample
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

object Distribution extends LowPriorityImplicits {
  def fromTable[A](weightsByItem: Map[A, Double]) = 
    EmpiricalTable(weightsByItem: Map[A, Double])
  
  def fromSequence[A](items: IndexedSeq[A]): Distribution[A] = 
    EmpiricalSeq(items)
  
  def build[A](f: Random => A): Distribution[A] = 
    Build(f)
  
  def exponential(rate: Double): Distribution[Double] = 
    build{(r: Random) => 
      - math.log(r.nextDouble) / rate
    }
}

trait LowPriorityImplicits {
  implicit val instances = new Samplable[Distribution] {
    override def pure[A](a: A): Distribution[A] 
      = Pure(a)
    
    override def flatMap[A,B](e: Distribution[A])(f: A => Distribution[B]): Distribution[B] 
      = FlatMap(e, f)
    
    override def sample[A](e: Distribution[A])(implicit r: Random) 
      = e.sample
    
    override def build[A](f: Random => A): Distribution[A] 
      = Build(f)
  }
}