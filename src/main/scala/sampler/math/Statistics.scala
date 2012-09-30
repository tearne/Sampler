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

package sampler.math

import sampler.data.Empirical
import sampler.data.FrequencyTable

/*
 * Mix in the StatisticsComponent to enable easy calculation of stats on Empirical
 */
trait StatisticsComponent{
	val statistics: Statistics = new Statistics
	
	class RichEmpirical[S,D](e: Empirical[S,D]){
		def mean(implicit f: Fractional[D]) = statistics.mean(e)
	}
	
	implicit def richEmpirical[S,D](e: Empirical[S,D]) = new RichEmpirical(e)
}

class Statistics{
	def mean[Domain](e: Empirical[_,Domain])(implicit num: Fractional[Domain]) = {
		import num._
		e.probabilityMap.foldLeft(0.0){case (acc, (v,p)) => {
			acc + v.toDouble * p.value
		}}
	}
}
