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
import java.util.concurrent.atomic.AtomicBoolean

case class Abort[T](af: Seq[Option[T]] => Boolean){
	def apply(soFar: Seq[Option[T]]) = af(soFar)
}

case class Job[T](f: () => Option[T]){
	def run(): Option[T] = f()
}
case class AbortableJob[T](f: AtomicBoolean => Option[T]){
	def run(stillRunning: AtomicBoolean): Option[T] = f(stillRunning)
}

trait JobRunner{
	def apply[T](jobs: Seq[Job[T]]): Seq[Option[T]]
}

trait AbortableRunner extends JobRunner{
	def apply[T](abort: Abort[T])(jobs: Seq[AbortableJob[T]]): Seq[Option[T]]
	
	def apply[T](jobs: Seq[Job[T]]): Seq[Option[T]] = 
		apply(
				//Never abort
				Abort((i:Seq[Option[T]]) => false)
		)(
				//Convert Job into pretend abortable job
				jobs.map(j => AbortableJob((b:AtomicBoolean) => j.run()))
		)
}

//class SerialRunner extends AbortableRunner{
//	def apply[T](abort: Seq[Option[T]] => Boolean)(jobs: Seq[() => Option[T]]): Option[Seq[Option[T]]] = {
//		@tailrec
//		def doJobsFrom(idx: Int, acc: Seq[Option[T]]): Option[Seq[Option[T]]] = {
//			if(idx == jobs.size) Some(acc.reverse)
//			else if(abort(acc)) None
//			else {
//				doJobsFrom(idx + 1, jobs(idx)() +: acc)
//			}
//		}
//		
//		doJobsFrom(0, Nil)
//	}
//}

class ParallelCollectionRunner extends JobRunner{
	def apply[T](jobs: Seq[Job[T]]): Seq[Option[T]] = {
		jobs.par.map(_.run).seq
	}
}