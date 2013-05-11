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

package sampler.run.akka.test

import sampler.run.UserInitiatedAbortException
import sampler.run.akka.worker.Executor
import sampler.run.akka.NodeApplication

object TestSlave extends App{
	new NodeApplication(new TestRunner())
}

class TestRunner() extends Executor{
	def run = PartialFunction[Any, Any]{
		case request: TestJob => 
			if(request.i == 3) throw new RuntimeException("Induced exception")
			(1 to 10000000).foreach{j => {
				if(isAborted) throw new UserInitiatedAbortException("Abort flag set")
				math.sqrt(j.toDouble)
			}}
			"Done"+request.i
	}
}