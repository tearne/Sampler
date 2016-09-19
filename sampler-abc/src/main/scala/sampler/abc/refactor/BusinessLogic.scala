package sampler.abc.refactor

import akka.actor.ActorRef
import sampler.abc.Weighted
import sampler.abc.actor.main.{MixPayload, ScoredParticles, WeighedParticles}
import sampler.abc.refactor.Messages.Start

class BusinessLogic[P] {
  def start(startMsg: Start[P], requestor: ActorRef): State[P] ={
    // Build initial state
    // start mixing timer
    //
    ???
  }

  def workerFailed(state: State[P], worker: ActorRef): Unit = {
    // Log the error
    // Allocate a repeat job to the same worker
  }

  def addNewLocallyScoredParticles(state: State[P], scored: ScoredParticles[P], sender: ActorRef): State[P] = {
    // filter and queue the new scored particles
    // report on the new state
    // send a weigh job to the worker that send the particles
    ???
  }

  def addNewWeighted(state: State[P], weighed: WeighedParticles[P], sender: ActorRef): State[P] = {
    // add the weighted particles to the state
    // do a report
    // If state.dueToFlush
    //       tell all local worker to abort via the broadcaster
    //       report gen finished
    //       tell the flusher to flush
    // Else
    //       nothing
    //return the new state
    ???
  }

  def doMixing(state: State[P]): Unit {
    // via the broadcaster
  }

  def addScoredFromMixPayload(mixP: MixPayload[P], sender: ActorRef): State = {
    
  }

}
