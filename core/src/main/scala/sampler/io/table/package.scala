package sampler.io

import sampler.math.Probability

package object table {

  	implicit val IntColumn: ColumnType[Int] = new ColumnType[Int] { def apply(s: String) = s.toInt }
	implicit val DoubleColumn: ColumnType[Double] = new ColumnType[Double] { def apply(s: String) = s.toDouble }
	implicit val BooleanColumn: ColumnType[Boolean] = new ColumnType[Boolean] { def apply(s: String) = s.toBoolean }
	implicit val StringColumn: ColumnType[String] = new ColumnType[String] { def apply(s: String) = s }
	implicit val FactorColumn: ColumnType[Factor] = new ColumnType[Factor] { def apply(s: String) = Factor(s) }
	implicit val ProbabilityColumn: ColumnType[Probability] = new ColumnType[Probability] { 
		def apply(s: String) = Probability(s.toDouble)
		override def toString(p: Probability) = p.value.toString
	}
}