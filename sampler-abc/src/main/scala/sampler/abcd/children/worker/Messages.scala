package sampler.abcd.children.worker

import sampler.abcd.ABCConfig
import sampler.abcd.generation.Generation

case class GenerateParticleFrom[P](prevGen: Generation[P], config: ABCConfig)
