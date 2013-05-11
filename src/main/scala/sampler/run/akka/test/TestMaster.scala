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

import sampler.run.ActorJob
import sampler.run.akka.FailFastRunner
import sampler.run.akka.PortFallbackSystem

case class TestJob(i: Int) extends ActorJob[String]

object TestMaster extends App{
	val system = PortFallbackSystem.systemWithPortFallback("ClusterSystem")
	
	val jobs = (1 to 10).map{i => TestJob(i)}
	
	val result = new FailFastRunner(system).apply(jobs)
	
	println("*********************")
	println("Result is ..."+result)
	println("*********************")
	
}