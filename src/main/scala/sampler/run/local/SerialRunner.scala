/*
 * Copyright (c) 2012-13 Crown Copyright 
 *                       Animal Health and Veterinary Laboratories Agency
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

package sampler.run.local

import scala.util.Try
import scala.annotation.tailrec

class SerialRunner(aborter: Aborter) extends LocalRunner{
	def apply[T](jobs: Seq[Abortable[T]]): Seq[Try[T]] = {
		val indexedJobs = jobs.toIndexedSeq
		
		@tailrec
		def doJobsFrom(idx: Int, acc: Seq[Try[T]]): Seq[Try[T]] = {
			if(idx == jobs.size) acc.reverse
			else {
				doJobsFrom(idx + 1, Try(indexedJobs(idx).run(aborter)) +: acc)
			}
		}
		
		doJobsFrom(0, Nil)
	}
}

object SerialRunner{
	def apply() = new SerialRunner(Aborter())
}