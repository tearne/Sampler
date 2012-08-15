package sampler.data

class TableHeader[T](val name: String)(implicit val m: Manifest[T])
class TableColumn[T](val values: IndexedSeq[T], val name: Option[String] = None)(implicit val m: Manifest[T])

class TableColumnMatcher[T](implicit desired: Manifest[T]){
	def unapply(tc: TableColumn[_]): Option[TableColumn[T]] = {
		if(tc.m == desired) Some(tc.asInstanceOf[TableColumn[T]])
		else None
	}
}
object TableColumnMatcher{
	lazy val IntTC = new TableColumnMatcher[Int]
	lazy val DoubleTC = new TableColumnMatcher[Double]
}

