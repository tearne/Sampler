package sampler.abc.actor.message

import sampler.abc.Generation

case class Start[P](initGeneration: Generation[P])

case object Failed

case object MixNow
case class MixPayload[P](scoredParticles: ScoredParticles[P])
case object ReportCompleted
