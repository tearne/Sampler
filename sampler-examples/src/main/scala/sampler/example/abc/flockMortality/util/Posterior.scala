package sampler.example.abc.flockMortality.util

import sampler.Implicits.RichIndexedSeq
import sampler.math.Statistics.quantile
import play.api.libs.json.JsLookupResult.jsLookupResultToJsLookup
import play.api.libs.json.JsValue
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.Writes
import play.api.libs.json.Json
import play.api.libs.json.JsObject

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
  
  def getMedian(posterior: Posterior): Parameters = {
    val medBeta = quantile(posterior.beta.toEmpiricalSeq, 0.5)
    val medEta = quantile(posterior.eta.toEmpiricalSeq, 0.5)
    val medGamma = quantile(posterior.gamma.toEmpiricalSeq, 0.5)
    val medDelta = quantile(posterior.delta.toEmpiricalSeq, 0.5)
    val medSigma = quantile(posterior.sigma.toEmpiricalSeq, 0.5)
    val medSigma2 = quantile(posterior.sigma2.toEmpiricalSeq, 0.5)
    val medOffset = posterior.offset.transpose.map(i =>
      quantile(i.map(_.toDouble).toEmpiricalTable, 0.5).toInt)
      
    // Create parameter object to use in model
    Parameters(medBeta, medEta, medGamma, medDelta, medSigma, medSigma2, medOffset)
    
  }
  
}