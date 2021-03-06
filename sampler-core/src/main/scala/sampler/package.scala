import sampler.distribution.DistributionImplicits
import sampler.samplable.SamplableSyntax
import sampler.samplable.WithoutReplacementImplicits
import sampler.empirical.EmpiricalImplicits
import sampler.io.{Meta, Rounding}
import sampler.maths.RangeCheckImplicits

package object sampler   
  extends SamplableSyntax
  with DistributionImplicits
  with WithoutReplacementImplicits
  with EmpiricalImplicits 
  with RangeCheckImplicits
  with Rounding
  with Meta
