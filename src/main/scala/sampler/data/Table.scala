package sampler.data

object HeaderImplicits{
	abstract class Parser[T]{ def apply(s: String):T }
	implicit val IntColumn = new Parser[Int] { def apply(s: String) = s.toInt }
	implicit val DoubleColumn = new Parser[Double] { def apply(s: String) = s.toDouble }
	implicit val BooleanColumn = new Parser[Boolean] { def apply(s: String) = s.toBoolean }
	implicit val StringColumn = new Parser[String] { def apply(s: String) = s }
}

import HeaderImplicits._
//TODO can do with multiple context bounds?
case class Header[T](val name: String)(implicit val parse: Parser[T], val m: Manifest[T])  
case class Column[T](val values: IndexedSeq[T], val name: Option[String] = None)(implicit val m: Manifest[T])

//TODO can do with context bound?
class ColumnMatcher[T](implicit desired: Manifest[T]){
	def unapply(tc: Column[_]): Option[Column[T]] = {
		if(tc.m == desired) Some(tc.asInstanceOf[Column[T]])
		else None
	}
}
object ColumnMatcher{
	lazy val IntTC = new ColumnMatcher[Int]
	lazy val DoubleTC = new ColumnMatcher[Double]
}

