package sampler.abc.refactor

import akka.actor.ActorRef
import sampler.abc.Weighted
import sampler.abc.actor.main.{MixPayload, ScoredParticles, WeighedParticles}
import sampler.abc.refactor.Messages.Start

class BusinessLogic[P] {
  def buildInitialState(startMsg: Start[P], requestor: ActorRef): State[P] ={
    ???
  }

  def workerFailed(state: State[P], worker: ActorRef): Unit = {
    // Log the failure
    // Allocate a repeat job to the same worker
  }

  def addLocallyScoredParticles(state: State[P], scored: ScoredParticles[P], sender: ActorRef): State[P] = {
    // Filter and queue the new scored particles
    // Report on the new state
    // Send a weigh job to the worker that sent the particles
    ???
  }

  def addScoredFromMixing(mixP: MixPayload[P], sender: ActorRef): State[P] = {
    // Update state by adding good particles
    // Report new state
    ???
  }

  def addNewWeighted(state: State[P], weighed: WeighedParticles[P], sender: ActorRef): State[P] = {
    // Add the weighted particles to the state
    // Do a report
    ???
  }

  def doGenerationFlush(state: State[P]) {
    // Tell all local workers to abort via the broadcaster
    // Report gen finished
    // Tell the flusher to flush newState
  }

  def allocateWork(state: State[P], worker: ActorRef) {
    //if stuff waiting to be weighed
    //  allocate weighing job
    //else
    //  allocate particle generating job
  }

  def doMixing(state: State[P]) {
    // via the broadcaster
  }

  def terminate(state: State[P]) {
    // Send Abort to all workers
    // Send message to generate a report
  }

  def startNewGeneration(state: State[P]) {
    // Send GenerateParticlesFrom message to workers via broadcaster
    // Send message to generate a report
  }

  def sendResultToClient(state: State[P]) {

  }
 }
