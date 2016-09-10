package sampler.abc.refactor

import akka.actor.Actor
import sampler.abc.Generation

object Messages {
  case class Start[P](initGen: Generation[P])
}

class RootActor[P](businessLogic: BusinessLogic) extends Actor {
  import Messages._

  def receive: Receive = {
    case s: Start[P] =>
      businessLogic.init(s.initGen)
  }


}

class BusinessLogic{

}