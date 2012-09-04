package sampler.data

import sampler.math.Probability

object Types {
	case class Header[T](val name: String)(implicit val cType: ColumnType[T])  
	case class Column[T](val values: Seq[T], val name: Option[String] = None)(implicit val cType: ColumnType[T]){
		def toStringColumn(): Column[String] = Column(values.map(v => cType.toString(v)), name)
	}
//	object Column{
//		def apply(values: Seq[T], val name: String) = Column(values, Some(name))
//	}
	
	case class Factor(name: String){
		override def toString() = name
		
//		TODO factor could be used to check inputs in ANOVA table and error if unexpected factor read in
	}
	
	abstract class ColumnType[T: Manifest]{ 
		val m = manifest[T]
		def apply(s: String):T
		def toString(value: T): String = value.toString
		def unapply(col: Column[_]): Option[Column[T]] = {
			if(col.cType.m == m) Some(col.asInstanceOf[Column[T]])
			else None
		}
	}
	implicit val IntColumn = new ColumnType[Int] { def apply(s: String) = s.toInt }
	implicit val DoubleColumn = new ColumnType[Double] { def apply(s: String) = s.toDouble }
	implicit val BooleanColumn = new ColumnType[Boolean] { def apply(s: String) = s.toBoolean }
	implicit val StringColumn = new ColumnType[String] { def apply(s: String) = s }
	implicit val FactorColumn = new ColumnType[Factor] { def apply(s: String) = Factor(s) }
	implicit val ProbabilityColumn = new ColumnType[Probability] { 
		def apply(s: String) = Probability(s.toDouble)
				override def toString(p: Probability) = p.value.toString
	}
}