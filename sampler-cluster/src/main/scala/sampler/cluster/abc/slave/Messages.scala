package sampler.cluster.abc.slave

import scala.language.existentials
import sampler.cluster.abc.ABCJob
import akka.actor.ActorRef

case class Abort(id: Int)
case class StatusRequest()
case class WorkerBusy()
case class WorkerIdle()
case class WorkConfirmed()
case class WorkRejected()
case class IndexedJob(job: ABCJob[_], id: Int)
