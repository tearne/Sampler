/*
 * Copyright (c) 2013 Crown Copyright 
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

package sampler.spike

import scala.annotation.tailrec
import scala.collection.GenSeq

object NumaTest extends App{
	
	val values = {
		val t = (1 to 1000000).map(_.toDouble)
		args(0) match{
			case "par" => t.par
			case _ | "seq" => t
		}
	}
	
	@tailrec
	def doMaps(values: GenSeq[Double], loopsRemaining: Int = 1000): GenSeq[Double] = {
		if(loopsRemaining == 0) values
		else doMaps(values.map(_ / math.E), loopsRemaining - 1)
	}

	val start = System.currentTimeMillis()
	doMaps(values)
	val duration = System.currentTimeMillis() - start
	
	println(f"done in ${duration/1000.0}%.2f seconds")
}