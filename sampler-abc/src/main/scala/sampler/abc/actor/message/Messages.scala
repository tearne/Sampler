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

package sampler.abc.actor.message

import sampler.abc.core.Generation

case class Start[P](generationZero: Generation[P])
case class Report[P](generationId: Int, tolerance: Double, posterior: Seq[P])

case class ReportCompleted[P](report: Report[P])

case class MixPayload[P](scoredParticles: ScoredParticles[P])
case object Failed

case class Abort()
case class Aborted()
