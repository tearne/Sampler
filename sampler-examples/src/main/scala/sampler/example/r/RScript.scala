package sampler.example.r

import java.nio.file.Paths

import org.apache.commons.io.FileUtils
import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
import org.apache.commons.math3.ode.nonstiff.DormandPrince853Integrator
import org.apache.commons.math3.ode.sampling.{FixedStepHandler, StepNormalizer}
import play.api.libs.json.Json

import scala.collection.mutable

object RScript extends App {

  //TODO Unfinished
  throw new UnsupportedOperationException("TODO example unfinished")

  // Checking that commons math3 ODE agrees with CRAN 'deSolve'

  case class Params(
      beta: Double,
      eta: Double,
      gamma: Double,
      delta: Double,
      endTime: Int
  ){
    def toJson: String = Json.prettyPrint(Json.obj(
      "beta" -> beta,
      "eta" -> eta,
      "gamma" -> gamma,
      "delta" -> delta,
      "endTime" -> endTime
    ))
  }

  case class State(
    S: Double,
    E: Double,
    I: Double,
    R: Double,
    D: Double
  )

  case class Solver(p: Params) extends FirstOrderDifferentialEquations {
    def getDimension: Int = 5

    def computeDerivatives(time: Double, y: Array[Double], yDot: Array[Double]): Unit = {
      import p._

      yDot(0) = -beta * y(0) * y(2)
      yDot(1) = beta * y(0) * y(2) - eta * y(1)
      yDot(2) = eta * y(1) - gamma * y(2)
      yDot(3) = (1 - delta) * gamma * y(2)
      yDot(4) = delta * gamma * y(2)
    }

    def solve(y0: Array[Double]): List[State] = {
      val timeZero = 0.0
      val relTol = 1e-11
      val absTol = 1e-11
      val minStep = 1e-8
      val maxStep = 100.0

      val steps = mutable.Buffer[State]()

      val stepHandler = new FixedStepHandler() {
        def init(t0: Double, y0: Array[Double], t: Double) {}

        def handleStep(t: Double, y: Array[Double], yDot: Array[Double], isLast: Boolean) {
          val state = State(
            y(0),
            y(1),
            y(2),
            y(3),
            y(4)
          )
          steps.append(state)
        }
      }

      val stepNormaliser = new StepNormalizer(1.0, stepHandler)
      val dp853 = new DormandPrince853Integrator(minStep, maxStep, absTol, relTol)
      val out = Array[Double](0, 0, 0, 0, 0) // Not used, since the step handler gets the output
      dp853.addStepHandler(stepNormaliser)
      dp853.integrate(this, 0.0, y0, params.endTime, out)

      steps.toList
    }
  }

  val y0 = Array[Double](
    2999, // S
    1,    // E
    0,    // I
    0,    // R
    0)    // D

  val params = Params(
    0.05, // beta
    0.25, // eta
    0.8,  // gamma
    0.1,  // delta
    10    // endDay
  )

  val commonsIntegrated = new Solver(params).solve(y0)

  val wDir = Paths.get("results", "RScript")
  FileUtils.writeStringToFile(
    wDir.resolve("params.json").toFile,
    params.toJson)

  val script = s"""
lapply(c("ggplot2", "reshape2", "deSolve", "jsonlite"), require, character.only=T)
pdf("test.pdf", width=4.13, height=2.91)

params = fromJSON("params.json")

Y0 = c(
  S = 2999,
  E = 1,
  I = 0,
  R = 0,
  D = 0
)

dY <-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    dS <- -beta * S * I
    dE <- beta * S * I - eta * E
    dI <- eta * E - gamma * I
    dR <- (1 - delta) * gamma * I
    dD <- delta * gamma * I

    list(c(dS, dE, dI, dR, dD))
  })

times = seq(0, 10, by = 1)

out <- ode(y = Y0, times = times, func = dY, parms = params)
head(out)
"""

//  RScript(script, wDir.resolve("script.R"))
}
