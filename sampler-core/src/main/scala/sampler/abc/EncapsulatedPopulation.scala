package sampler.abc

abstract class EncapsulatedPopulation[M <: ABCModel] {
//	type M <: ABCModel
	val model : M
	val population : Seq[model.Weighted]//model.Population
}

object EncapsulatedPopulation {
  def apply[M <: ABCModel](model0 : M)(population0 : Seq[model0.Weighted]) =
    new EncapsulatedPopulation[M] {
      type M = model0.type	//Why do we need this when it's defined in the type bounds of apply?
      val model : M = model0
      val population = population0
    }
}