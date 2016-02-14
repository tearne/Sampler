package sampler.spike.distribution.tearne

import cats.Monad
import scala.language.higherKinds
import sampler.math.{Random, Partition, AliasTable}
import scala.annotation.tailrec
import scala.collection.{GenSeq, GenMap}

//TODO Applicative

trait Samplable[F[_]]{
  def sample[A](fa: F[A])(implicit r: Random): A
}
object Samplable{
  def apply[F[_]](implicit s: Samplable[F]): Samplable[F] = s
}

sealed trait Empirical[A]{
  def sample(implicit r: Random): A
  def flatMap[B](f: A => Empirical[B]) = FlatMap(this, f)
  def map[B](f: A => B) = FlatMap(this, (a: A) => Pure(f(a)))
  
  def until(predicate: IndexedSeq[A] => Boolean) = Until(this, predicate)
  def filter(predicate: A => Boolean) = Filter(this, predicate)
  def combine[B, C](that: Empirical[B])(op: (A, B) => C) = Combine(this, that, op)
}

final case class Pure[A](value: A) extends Empirical[A] {
  def sample(implicit r: Random) = value
}

final case class FlatMap[A,B](d: Empirical[A], f: A => Empirical[B]) extends Empirical[B]{
  def sample(implicit r: Random) = f(d.sample).sample
}

final case class Until[A](
    d: Empirical[A], 
    predicate: IndexedSeq[A] => Boolean
    ) extends Empirical[IndexedSeq[A]] {
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
    d: Empirical[A], 
    predicate: A => Boolean
    ) extends Empirical[A] {
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
    dA: Empirical[A], 
    dB: Empirical[B], 
    f: (A,B) => C
    ) extends Empirical[C] {
  def sample(implicit r: Random) = f(dA.sample, dB.sample)
}

final case class TableDist[A](weightsByItem: Map[A, Double]) extends Empirical[A]{
  val (items, weights) = weightsByItem.toIndexedSeq.unzip
  assume(weights.find(_ <= 0).isEmpty, "Found negative weights when building distribution.")
  val probPartition = Partition.fromWeights(weights)
  val aliasTable = new AliasTable(probPartition)
  def sample(implicit r: Random) = items(aliasTable.next(r))
}

final case class SeqDist[A](items: IndexedSeq[A]) extends Empirical[A]{
  val size = items.size
  def sample(implicit r: Random) = items(r.nextInt(size))
}

object Empirical {
  implicit val empiricalInstances = new Samplable[Empirical] with Monad[Empirical]{
    override def pure[A](a: A): Empirical[A] = Pure(a)
    override def flatMap[A,B](e: Empirical[A])(f: A => Empirical[B]): Empirical[B] = e.flatMap(f)
    override def sample[A](e: Empirical[A])(implicit r: Random) = e.sample
  }
}

object EmpiricalSyntax {
  implicit class SeqOps[A](genSeq: GenSeq[A]) {
		def empirical = SeqDist[A](genSeq.toIndexedSeq)
	}
	
	implicit class MapOps[A](table: GenMap[A,Double]) {
		def empirical = TableDist[A](table.seq.toMap)
	}
}

object Discussion extends App {
  val table: Map[String, Double] = Map(
    "Pig" -> 10,
    "Duck" -> 20,
    "Cow" -> 30
  )
  val observations = Seq("Milk", "Dark", "Dark", "Milk", "Milk")
  
  import EmpiricalSyntax._
  val chocDist = observations.empirical
  val animalDist = table.empirical
  
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