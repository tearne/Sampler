package sampler.abc.actor.root.state.task.egen

import scala.collection.immutable.Queue

class ObservedIdsTrimmer(maxParticleMemory: Int) {
  def apply(queuedIds: Queue[Long]) = {
    val queueSize = queuedIds.size

    if (queueSize >= maxParticleMemory) {
      val reducedNum: Int = (maxParticleMemory * 0.9).toInt
      val toDrop = queueSize - reducedNum
      queuedIds.drop(toDrop)
    }
    else queuedIds
  }
}