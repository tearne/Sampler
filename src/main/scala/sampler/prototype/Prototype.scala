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

package sampler.prototype

import shapeless.HList
import java.nio.file.Path
import shapeless.Sized
import shapeless.Nat
import scala.collection.IndexedSeqLike
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.mutable.ListBuffer

trait Probability{
	def sample
}
object Probability{
	def apply(value: Double): Probability = null
}
class ProbabilityException extends Exception

trait Random extends scala.util.Random

trait Model[Conf, In, Out] extends (Conf => In => Out)

trait Optimise[Conf, Domain, Value <: Ordered[Value]] extends (Conf => (Domain, Value))
trait Sweep[Conf, Domain, Value] extends (Conf => Traversable[(Domain, Value)])

trait Runner[Result]{
	def run(job: Seq[() => Result]): IndexedSeq[Result]
}

trait Samplable[T]{
	def sample(implicit rand: Random): T
}

trait Empirical[T] extends Samplable[T]{
	def +(item: T): Unit
	def ++(items: TraversableOnce[T]): Unit
	def +(that: Empirical[T]): Empirical[T]
	
	def apply(obs: T): Int
	def relativeFreq(obs: T): Probability
	def counts(): Map[T, Int]

	def filter(predicate: T => Boolean): Empirical[T]
	
	def toDistribution(implicit order: Ordering[T]): Distribution[T]
	
	def map[B](f: T => B): Empirical[B]
	def flatMap[B](f: T => Empirical[B]): Empirical[B]
}

trait Distribution[T] extends Empirical[T]{
	def cdf(elem: T): Double 
	def quantile(p: Probability): T
}

trait EmpiricalMetric[T]{
	def distance(a: Empirical[T], b: Empirical[T])
}

//
// Handling tables of data
//

class TableHeader[T](val name: String)(implicit val m: Manifest[T])
class TableColumn[T](val values: IndexedSeq[T], val name: Option[String] = None)(implicit val m: Manifest[T])

class TableColumnMatcher[T](implicit desired: Manifest[T]){
	def unapply(tc: TableColumn[_]): Option[TableColumn[T]] = {
		if(tc.m == desired) Some(tc.asInstanceOf[TableColumn[T]])
		else None
	}
}
object TableColumnMatcher{
	lazy val IntTC = new TableColumnMatcher[Int]
	lazy val DoubleTC = new TableColumnMatcher[Double]
}

class Def[C](implicit desired : Manifest[C]) {
	def unapply[X](c : X)(implicit m : Manifest[X]) : Option[C] = {
		println("unapply "+m.toString+", "+desired.toString+", "+c.toString)
		def sameArgs = desired.typeArguments.zip(m.typeArguments).forall {case (desired,actual) => desired >:> actual}
		if (desired >:> m && sameArgs) Some(c.asInstanceOf[C])
     	else None
     }
}

trait TableReader{
	def get[T](params: TableHeader[T]): IndexedSeq[T]
}
trait TableWriter{
	def apply(path: Path, overwrite: Boolean = false, append: Boolean = false)(params: IndexedSeq[_]*): Unit
}