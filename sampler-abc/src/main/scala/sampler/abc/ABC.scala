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

package sampler.abc

import java.util.concurrent.TimeUnit.MILLISECONDS

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import sampler.abc.actor.children.flushing.{GenerationFlusher, ObservedIdsTrimmer, ToleranceCalculator}
import sampler.abc.actor.root._
import sampler.abc.refactor.{ChildActors, RootActor}
import sampler.cluster.PortFallbackSystemFactory
import sampler.io.Logging
import sampler.maths.Random

import scala.concurrent.Await
import scala.concurrent.duration.Duration

case class ActorStuff(rootActor: ActorRef, system: ActorSystem)

trait ABCActors {
  def initActorStuff[P](
      model: Model[P],
      config: ABCConfig,
      generationHandler: Option[Population[P] => Unit]
  ): ActorStuff
}

trait ABCActorsImpl extends ABCActors {
  def initActorStuff[P](
      model: Model[P],
      config: ABCConfig,
      reportHandler: Option[Population[P] => Unit]
  ) = {

    val system = PortFallbackSystemFactory(config.clusterName)

    //TODO this is mess.  Tidy the factory floor!
    val random = Random
    val getters = new Getters()

    lazy val helper = new Helper(
      new ParticleMixer(),
      getters,
      random)

    val businessLogic = new BusinessLogic(
      helper,
      config,
      getters
    )

    lazy val generationFlusher = new GenerationFlusher(
      ToleranceCalculator,
      new ObservedIdsTrimmer(config.memoryGenerations,config.numParticles),
      getters,
      config)

    val childActorsFactory = new ChildActors(
      generationFlusher,
      config,
      model,
      reportHandler,
      random
    )
    val rootActor = system.actorOf(
      Props(classOf[RootActor[P]],
        childActorsFactory,
        businessLogic,
        config),
      "root")

    ActorStuff(rootActor, system)
  }
}

object ABC extends ABCActorsImpl with Logging {
  def apply[P](
      model: Model[P],
      config: ABCConfig): Population[P] =
    apply(model, config, None, UseModelPrior[P]())

  def apply[P](
      model: Model[P],
      config: ABCConfig,
      genHandler: Population[P] => Unit): Population[P] =
    apply(model, config, Some(genHandler), UseModelPrior())

  def resume[P](
      model: Model[P],
      config: ABCConfig,
      population: Population[P]
  ): Population[P] = {
    apply(model, config, None, population)
  }

  def resume[P](
      model: Model[P],
      config: ABCConfig,
      population: Population[P],
      genHandler: Population[P] => Unit
  ): Population[P] = {
    apply(model, config, Some(genHandler), population)
  }

  def apply[P](
      model: Model[P],
      config: ABCConfig,
      genHandler: Option[Population[P] => Unit],
      initialPopulation: Generation[P]): Population[P] = {
    info("      Job config: " + config.renderJob)
    info("Algorithm config: " + config.renderAlgorithm)
    info("  Cluster config: " + config.renderCluster)

    val actorStuff = initActorStuff(model, config, genHandler)

    implicit val timeout = Timeout(config.futuresTimeoutMS, MILLISECONDS)
    val future = (actorStuff.rootActor ? Start(initialPopulation)).mapTo[Population[P]]
    val result = Await.result(future, Duration.Inf)
    //TODO unlimited timeout just for the future above?

    if (config.terminateAtTargetGen) {
      info("Terminating actor system")
      actorStuff.system.terminate
    }

    result
  }
}