package sampler.cluster.abc.master

import scala.language.existentials
import akka.actor.ActorRef
import sampler.cluster.abc.ABCJob

case class ClusterBusy()
case class WorkAvailable()
case class AbortAll(id: Int)
