package sampler.example.abc.flockMortality

import java.nio.file.Paths

import org.apache.commons.io.FileUtils
import play.api.libs.json.Json
import sampler.example.abc.flockMortality.util.{ODE, ODEState, Parameters}
import sampler._
import sampler.r.script.RScript

object ODETest extends App {

  val S0 = 3000
  val E0 = 1
  val D0 = 0
  val y0 = Array(S0, E0, 0.0, 0.0, D0)


  val numDays = 10
  val params = Parameters(
    0.05, //beta
    0.25,//eta
    0.8, //gamma
    0.1, //delta
    0.5, //sigma
    0.4,  //sigma2,
    IndexedSeq(9)  //offset
  )
  val eggCoeff = 0.8
  val baselineMortalityRate = 0

  val integrated = //ODEState.rounded(
    ODE.run(
      y0,
      numDays,
      params,
      eggCoeff,
      baselineMortalityRate
    )
  //1)

  val jsonStr = Json.prettyPrint(Json.obj(
    "day" -> Json.toJson(0 until integrated.size),
    "s" -> Json.toJson(integrated.map(_.s.decimalPlaces(2))),
    "e" -> Json.toJson(integrated.map(_.e.decimalPlaces(2))),
    "i" -> Json.toJson(integrated.map(_.i.decimalPlaces(2))),
    "r" -> Json.toJson(integrated.map(_.r.decimalPlaces(2))),
    "d" -> Json.toJson(integrated.map(_.d.decimalPlaces(2)))
  ))

  val wDir = Paths.get("results", "odeTest")
  FileUtils.writeStringToFile(wDir.resolve("solved.json").toFile, jsonStr)


  val script = """
lapply(c("ggplot2", "reshape2", "deSolve", "jsonlite"), require, character.only=T)
pdf("test.pdf", width=4.13, height=2.91)

params = c(
  beta = 0.05,
  eta = 0.25,
  gamma = 0.8,
  delta = 0.1,
  sigma = 0.5,
  sigma2 = 0.4
)

Y0 = c(
  S = 3000,
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
}

times = seq(0, 10, by = 1)

out <- ode(y = Y0, times = times, func = dY, parms = params)
head(out)
data = melt(as.data.frame(out), id="time")
ggplot(data, aes(x=time, y=value, colour=variable)) +
geom_line()


data = melt(as.data.frame(fromJSON("solved.json")), id="day")
ggplot(data, aes(x=day, y=value, colour=variable)) +
  geom_line()
dev.off()
  """

  RScript.apply(script, wDir.resolve("script.R"))
}
