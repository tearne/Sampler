//package sampler.abc.core
//
//import org.scalatest.FreeSpec
//import org.scalatest.Matchers
//import sampler.math.StatisticsComponent
//import sampler.math.Statistics
//import org.scalatest.mock.MockitoSugar
//import sampler.abc.Model
//import sampler.abc.Scored
//import sampler.abc.Prior
//import org.mockito.Mockito._
//import org.mockito.Matchers.anyInt
//import sampler.abc.Weighted
//import sampler.abc.actor.sub.flushing.WeightsHelper
//
//class WeightsHelperTest extends FreeSpec with Matchers with MockitoSugar {
//
//  val instance = new WeightsHelper()
////  erComponent with StatisticsComponent{
////    val statistics = Statistics
////    val weigher = new Weigher{}
////  }
//  
////  val instance = instanceComponent.weigher
//  
//  
//  
//  "Gives a consolidated weights table" in {
//    val population = Seq(
//        Weighted(Scored(1, Seq(0.1)), 0.1),
//        Weighted(Scored(1, Seq(0.2)), 0.2),
//        Weighted(Scored(2, Seq(0.3)), 0.3),
//        Weighted(Scored(2, Seq(0.4)), 0.4)
//    )
//    
//    val consolidatedMap = instance.consolidateToWeightsTable(population)
//    
//    val tolerance = 1e-6
//    
//    consolidatedMap.getOrElse(1, 0.0) should be(0.3 +- tolerance)
//    consolidatedMap.getOrElse(2, 0.0) should be(0.7 +- tolerance)
//  }
//}