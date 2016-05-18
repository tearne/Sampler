/*
 * Copyright (c) 2012-15 Crown Copyright 
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

package sampler.abc.actor.sub.flushing

import sampler.math.StatisticsComponent
import sampler.Implicits._
import sampler.math.StatisticsImpl
import sampler.io.Logging
import sampler.abc.ABCConfig
import sampler.math.Statistics
import sampler.abc.actor.main.Weighted

trait ToleranceCalculator extends StatisticsComponent with Logging {
	def apply[P](weighted: Seq[Weighted[P]], config: ABCConfig, currentTolerance: Double): Double = {
		val percentileMeanScore = statistics.quantile(weighted.map { _.meanScore }.toEmpiricalSeq, config.toleranceDescentPercentile)
		if (percentileMeanScore == 0) {
			warn("New tolerance evaluated to 0. Will use old tolerance again.")
			currentTolerance
		} else if (percentileMeanScore > currentTolerance) {
			warn("New tolerance is greater than old tolerance. Will use old tolerance again.")
			currentTolerance
		} else percentileMeanScore
	}
}

object ToleranceCalculator
	extends ToleranceCalculator
	with StatisticsImpl