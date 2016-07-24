import sampler.distribution.DistributionImplicits
import sampler.samplable.SamplableSyntax
import sampler.samplable.WithoutReplacementImplicits
import sampler.empirical.EmpiricalImplicits

package object sampler   
  extends SamplableSyntax
  with DistributionImplicits
  with WithoutReplacementImplicits
  with EmpiricalImplicits 
  {
}