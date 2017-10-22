package sampler.distribution

import sampler.maths.{AliasTable, Random}
import sampler.samplable.Samplable

import scala.annotation.tailrec
import scala.language.higherKinds

//TODO Applicative

/*
 *  Algebraic data type for a distribution
 */
trait Distribution[A] {
  def sample(implicit r: Random): A
  //TODO canEqual, equals, hashcode etc  (monad?)
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

final case class EmpiricalTable[A](items: IndexedSeq[A], aliasTable: AliasTable) extends Distribution[A] {
  assert(items.size == aliasTable.size)
  def sample(implicit r: Random) = items(aliasTable.next(r))
}
object EmpiricalTable{
  def apply[A](weightsByItem: Map[A, Double]): EmpiricalTable[A] = {
    val (items, weights) = weightsByItem.toIndexedSeq.unzip
    assume(weights.exists(_ > 0), "No positive value in weights")
    assume(weights.find(_ < 0).isEmpty, "Found negative weights.")
    val totalWeight = weights.sum
    val probabilities = weights.map(_ / totalWeight)
    EmpiricalTable(items, new AliasTable(probabilities))
  }
}


final case class EmpiricalSeq[A](items: IndexedSeq[A]) extends Distribution[A] {
  val size = items.size
  def sample(implicit r: Random) = items(r.nextInt(size))
}

object Distribution extends LowPriorityImplicits with CommonDistributions{
  def fromWeightsTable[A](weightsByItem: Map[A, Double]): Distribution[A] = 
    EmpiricalTable(weightsByItem: Map[A, Double])
  
  def fromSequence[A](items: IndexedSeq[A]): Distribution[A] = 
    EmpiricalSeq(items)
  
  def from[A](f: Random => A): Distribution[A] = 
    Build(f)
}

trait LowPriorityImplicits {
  // Instance of the Samplable type class for Distributions
  implicit val isSamplable = new Samplable[Distribution] {
    override def pure[A](a: A): Distribution[A] 
      = Pure(a)
    
    override def flatMap[A,B](e: Distribution[A])(f: A => Distribution[B]): Distribution[B] 
      = FlatMap(e, f)

    override def sample[A](e: Distribution[A])(implicit r: Random) 
      = e.sample
    
    override def from[A](f: Random => A): Distribution[A] 
      = Build(f)

    override def tailRecM[A, B](a: A)(f: (A) => Distribution[Either[A, B]]): Distribution[B] = {
      def untilB(d: Distribution[Either[A, B]]): Distribution[B] = Distribution.from{ random =>
        @tailrec def go(): B = {
          d.sample(random) match {
            case Left(a) => go()
            case Right(b) => b
          }
        }

        go()
      }

      untilB(f(a))
    }
  }
}