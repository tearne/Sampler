/*
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

package sampler.run

import scala.annotation.tailrec


trait JobRunner[T]{
	def apply(jobs: Seq[() => Option[T]]): Option[Seq[Option[T]]]
}

class SerialRunner[T](abort: Seq[Option[T]] => Boolean = (_: Seq[Option[T]]) => false) extends JobRunner[T]{
	def apply(jobs: Seq[() => Option[T]]): Option[Seq[Option[T]]] = {
		@tailrec
		def doJobsFrom(idx: Int, acc: Seq[Option[T]]): Option[Seq[Option[T]]] = {
			if(idx == jobs.size) Some(acc.reverse)
			else if(abort(acc)) None
			else {
				doJobsFrom(idx + 1, jobs(idx)() +: acc)
			}
		}
		
		doJobsFrom(0, Nil)
	}
}

class ParallelCollectionRunner[T] extends JobRunner[T]{
	def apply(jobs: Seq[() => Option[T]]): Option[Seq[Option[T]]] = {
		Some(jobs.par.map(_()).seq)
	}
}

class AkkaRunner[T](abort: Seq[Option[T]] => Boolean) extends JobRunner[T]{
	def apply(jobs: Seq[() => Option[T]]): Option[Seq[Option[T]]] = null
}

object Runner{
	def serial[T](abort: Seq[Option[T]] => Boolean = (_: Seq[Option[T]]) => false) = 
		new SerialRunner(abort)
	
	def parallelCollection[T]() = new ParallelCollectionRunner[T]()
	
	//TODO Akka runner
}