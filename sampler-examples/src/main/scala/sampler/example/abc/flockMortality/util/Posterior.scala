package sampler.example.abc.flockMortality.util


import play.api.libs.json.JsLookupResult.jsLookupResultToJsLookup
import play.api.libs.json.JsValue
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.Writes
import play.api.libs.json.Json
import play.api.libs.json.JsObject

import sampler._

case class Posterior(
    beta: IndexedSeq[Double], 
    eta: IndexedSeq[Double], 
    gamma: IndexedSeq[Double], 
    delta: IndexedSeq[Double], 
    sigma: IndexedSeq[Double], 
    sigma2: IndexedSeq[Double], 
    offset: IndexedSeq[IndexedSeq[Int]]
)
object Posterior{
  
  def apply(json: JsValue): Posterior = Posterior(
  (json \ "beta").as[List[Double]].toIndexedSeq,
  (json \ "eta").as[List[Double]].toIndexedSeq,
  (json \ "gamma").as[List[Double]].toIndexedSeq,    
  (json \ "delta").as[List[Double]].toIndexedSeq,
  (json \ "sigma").as[List[Double]].toIndexedSeq,
  (json \ "sigma2").as[List[Double]].toIndexedSeq,
  (json \ "offset").as[List[List[Int]]].toIndexedSeq.map(i => i.toIndexedSeq)
  )
  
  implicit val posteriorWrites = new Writes[Posterior] {
    def writes(data: Posterior) = { Json.obj(
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
  
  def fromSeq(paramSeq: IndexedSeq[Parameters]) = Posterior(
    paramSeq.map(_.beta),
    paramSeq.map(_.eta),
    paramSeq.map(_.gamma),
    paramSeq.map(_.delta),
    paramSeq.map(_.sigma),
    paramSeq.map(_.sigma2),
    paramSeq.map(_.offset)
  )
  
  def getMarginalMedian(posterior: Posterior): Parameters = {
    val medBeta = posterior.beta.toEmpirical.percentile(0.5)
    val medEta = posterior.eta.toEmpirical.percentile(0.5)
    val medGamma = posterior.gamma.toEmpirical.percentile(0.5)
    val medDelta = posterior.delta.toEmpirical.percentile(0.5)
    val medSigma = posterior.sigma.toEmpirical.percentile(0.5)
    val medSigma2 = posterior.sigma2.toEmpirical.percentile(0.5)
    val medOffset = posterior.offset.transpose.map(i =>
      i.map(_.toDouble).toEmpirical.percentile(0.5).toInt)
      
    // Create parameter object to use in model
    Parameters(medBeta, medEta, medGamma, medDelta, medSigma, medSigma2, medOffset)
    
  }
  
}