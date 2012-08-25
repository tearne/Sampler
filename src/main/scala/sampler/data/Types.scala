package sampler.data

object Types {
	case class Header[T](val name: String)(implicit val cType: ColumnType[T])  
	case class Column[T](val values: IndexedSeq[T], val name: Option[String] = None)(implicit val cType: ColumnType[T])
	
	case class Factor(name: String){
		override def toString() = "Factor: "+name
	}
	
	abstract class ColumnType[T: Manifest]{ 
		val m = manifest[T]
		def apply(s: String):T 
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
}