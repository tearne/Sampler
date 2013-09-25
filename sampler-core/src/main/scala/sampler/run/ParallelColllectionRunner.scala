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

package sampler.run

import scala.util.Try

class ParallelCollectionRunner(aborter: Aborter) extends Runner{
	def apply[T](jobs: Seq[Abortable[T]]): Seq[Try[T]] = {
		jobs.par.map(job => Try(job.run(aborter))).seq
	}
}
object ParallelCollectionRunner{
	def apply[T](jobs: Seq[Abortable[T]]) = new ParallelCollectionRunner(Aborter())(jobs)
	def apply[T]() = new ParallelCollectionRunner(Aborter())
}

object ParallelCollectionRunnerTest extends App{
	// Make a bunch of jobs one of which will trigger an abort
	val jobs = (1 to 10).map{id => Abortable[Double](aborter => {
		println(s"Start job $id")
		if(id == 4){
			println("Aborting the jobs")
			aborter.abort
		}
		
		val results = for{
			p <- 1 to 1000000
		} yield {
			if(aborter.isAborted) throw new DetectedAbortionException
			math.sqrt(math.Pi / p)
		}
		results.sum
	})}
	
	val aborter = Aborter()
	
	// Run the jobs
	ParallelCollectionRunner(jobs).foreach(println)
}