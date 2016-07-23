package sampler.statistical

import scala.language.higherKinds

trait Statistical[T[_]] {
	def observationCount(t: T[_]): Int
	def probabilityTable[A](t: T[A]): Map[A, Double]
}