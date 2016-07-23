import sampler.distribution.DistributionImplicits
import sampler.samplable.SamplableSyntax
import sampler.samplable.WithoutReplacementImplicits

package object sampler   
  extends SamplableSyntax
  with DistributionImplicits
  with WithoutReplacementImplicits
  //with StatisticalImplicits 
  {
}