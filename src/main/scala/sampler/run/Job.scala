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

import scala.annotation.tailrec
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.Try
import scala.concurrent.Promise
import scala.util.Success

case class Abortable[T](f: (Aborter) => T){
	def run(aborter: Aborter) = f(aborter)
}
trait LocalJobRunner{
	def apply[T](jobs: Seq[Abortable[T]]): Seq[Try[T]]
}


trait JobRequest[Ret]

class UserInitiatedAbortException(message: String = null, cause: Throwable = null) 
	extends RuntimeException(message, cause)

trait Aborter{
	def abort: Unit
	def isAborted: Boolean
}

class SimpleAborter extends Aborter{
	val b = new AtomicBoolean(false)
	def abort {b.set(true)}
	def isAborted = b.get()
}

class WrappedAborter(ab: AtomicBoolean) extends Aborter{
	def abort {ab.set(true)}
	def isAborted = ab.get()
}

trait ActorJob[T]
trait ActorJobRunner{
	def apply[T](jobs: Seq[ActorJob[T]]): Seq[Try[T]]
}

