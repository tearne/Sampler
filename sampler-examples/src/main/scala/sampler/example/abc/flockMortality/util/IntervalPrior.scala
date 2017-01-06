package sampler.example.abc.flockMortality.util

import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Writes
import sampler.math.Random

case class IntervalPrior(
    beta: List[Double],
    eta: List[Double],
    gamma: List[Double],
    delta: List[Double],
    sigma: List[Double],
    sigma2: List[Double],
    offset: List[Int]
)
object IntervalPrior {  
  def apply(json: JsValue): IntervalPrior = IntervalPrior(
    (json \ "params" \ "beta").as[List[Double]],
    (json \ "params" \ "eta").as[List[Double]],
    (json \ "params" \ "gamma").as[List[Double]],
    (json \ "params" \ "delta").as[List[Double]],
    (json \ "params" \ "sigma").as[List[Double]],
    (json \ "params" \ "sigma2").as[List[Double]],
    (json \ "params" \ "offset").as[List[Int]]
  )
  
  implicit val priorWrites = new Writes[IntervalPrior] {
    def writes(data: IntervalPrior) = { Json.obj(
          "type" -> "interval",
          "params" -> Json.obj(
            "beta" -> data.beta,
            "eta" -> data.eta,
            "gamma" -> data.gamma,
            "delta" -> data.delta,
            "sigma" -> data.sigma,
            "sigma2" -> data.sigma2,
            "offset" -> data.offset
           )
      )
    }
  }
  
  def density(params: Parameters, interval: IntervalPrior): Double = {
    def normalisedRange(d: Double, a: Double, b: Double) =
      if(d > b || d < a) 0.0 else 1.0 / math.abs(b - a)
    import params._
    val offsetProduct = offset.map(i => normalisedRange(i, interval.offset.head, interval.offset.last)).product
    normalisedRange(beta, interval.beta.head, interval.beta.last) *
    normalisedRange(eta, interval.eta.head, interval.eta.last) *
    normalisedRange(gamma, interval.gamma.head, interval.gamma.last) *
    normalisedRange(delta, interval.delta.head, interval.delta.last) *
    normalisedRange(sigma, interval.sigma.head, interval.sigma.last) *
    normalisedRange(sigma2, interval.sigma2.head, interval.sigma2.last) *
    offsetProduct
  }
  
  def draw(numSheds: Int, interval: IntervalPrior): Parameters = {
    import interval._
    val rand = Random
    val p = Parameters(
        beta = rand.nextDouble(beta.head, beta.last),
        eta = rand.nextDouble(eta.head, eta.last),
        gamma = rand.nextDouble(gamma.head, gamma.last),
        delta = rand.nextDouble(delta.head, delta.last),
        sigma = rand.nextDouble(sigma.head, sigma.last),
        sigma2 = rand.nextDouble(sigma2.head, sigma2.last),
        offset = (0 until numSheds).map(i => rand.nextInt(offset.last - offset.head) + offset.head).toIndexedSeq
      )
      assert(IntervalPrior.density(p, interval) > 0)    
      p
  }
  
}