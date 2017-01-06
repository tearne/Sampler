package sampler.example.abc.flockMortality.util

import sampler.io.Tokenable
import sampler.io.Tokens
import java.math.MathContext
import play.api.libs.json.JsValue
import play.api.libs.json.Writes
import play.api.libs.json.Json

case class Parameters(
    beta: Double, 
    eta: Double, 
    gamma: Double, 
    delta: Double, 
    sigma: Double, 
    sigma2: Double, 
    offset: IndexedSeq[Int]
){
  def toSeq = Seq(beta, eta, gamma, delta, sigma, sigma2, offset) 
}
object Parameters {
  
  def parser(toks: Map[String, JsValue]) = Parameters(
      toks("beta").as[Double],
      toks("eta").as[Double],
      toks("gamma").as[Double],
      toks("delta").as[Double],
      toks("sigma").as[Double],
      toks("sigma2").as[Double],
      toks("offset").as[IndexedSeq[Int]]
  )
  
  implicit val tokener: Tokenable[Parameters] = new Tokenable[Parameters] {
    val mc = new MathContext(6)
    def getTokens(p: Parameters) = Tokens.named(
      "beta" -> BigDecimal(p.beta, mc),
      "eta" -> BigDecimal(p.eta, mc),
      "gamma" -> BigDecimal(p.gamma, mc),
      "delta" -> BigDecimal(p.delta, mc),
      "sigma" -> BigDecimal(p.sigma, mc),
      "sigma2" -> BigDecimal(p.sigma2, mc),
      "offset" -> p.offset
    )
  }
  
  implicit val paramWrites = new Writes[Parameters] {
    def writes(data: Parameters) = { Json.obj(
          "beta" -> data.beta,
          "eta" -> data.eta,
          "gamma" -> data.gamma,
          "delta" -> data.delta,
          "sigma" -> data.sigma,
          "sigma2" -> data.sigma2,
          "offset" -> data.offset
      )
    }
  }
  
  val header = Seq("Beta", "Eta", "Gamma", "Delta", "Sigma", "Sigma2", "Offset")
  
}