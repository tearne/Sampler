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

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import com.typesafe.config.ConfigFactory
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import sampler.abc.actor.main.MainActorImpl
import sampler.cluster.PortFallbackSystemFactory
import sampler.io.Logging
import sampler.abc.actor.main.Start
import sampler.abc.actor.main.component.Helper
import sampler.abc.actor.main.component.helper.{Getters, ParticleMixer}
import sampler.abc.actor.sub.flushing.{GenerationFlusher, ObservedIdsTrimmer, ToleranceCalculator}
import sampler.abc.refactor.{BusinessLogic, ChildActors, RootActor}
import sampler.maths.Random

trait ABCActors {
  val system: ActorSystem

  def entryPointActor[P](
      model: Model[P],
      config: ABCConfig,
      generationHandler: Option[Population[P] => Unit]): ActorRef
}

trait ABCActorsImpl extends ABCActors {
  //TODO make actor system name configurable
  val system = PortFallbackSystemFactory("ABC")

  def entryPointActor[P](
      model: Model[P],
      config: ABCConfig,
      reportHandler: Option[Population[P] => Unit]) = {

    //TODO this is mess.  Tidy the factory floor!
    val random = Random
    val getters = new Getters()

    val businessLogic = new BusinessLogic[P](
      new Helper(
        new ParticleMixer(),
        new ToleranceCalculator {},
        getters,
        random
      ),
      config,
      getters
    )

    lazy val helper = new Helper(
      new ParticleMixer(),
      ToleranceCalculator,
      getters,
      random)

    lazy val generationFlusher = new GenerationFlusher(
      helper.toleranceCalculator,
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
    system.actorOf(
      Props(classOf[RootActor[P]],
        childActorsFactory,
        businessLogic,
        config),
      "root")
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

  def resumeByRepeatingTolerance[P](
      model: Model[P],
      config: ABCConfig,
      population: Population[P]
  ): Population[P] = {
    apply(model, config, None, population)
  }

  def resumeByRepeatingTolerance[P](
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

    val actor = entryPointActor(model, config, genHandler)

    implicit val timeout = Timeout(config.futuresTimeoutMS, MILLISECONDS)
    val future = (actor ? Start(initialPopulation)).mapTo[Population[P]]
    val result = Await.result(future, Duration.Inf)
    //TODO unlimited timeout just for the future above?

    if (config.terminateAtTargetGen) {
      info("Terminating actor system")
      system.terminate
    }

    result
  }
}