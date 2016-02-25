package sampler.spike.typeclass

object Equals extends App {
  /*
   *  It's bad to compare objects of different types
   */
  println(List(1,2,3).map(_ == "1"))  // false, false, false!
  
  /*
   * Eq.eqv(a, b) makes sure that a & b must be of the same type  
   */
  import cats.Eq
  import cats.std.all._
//  Eq[String].eqv("1", 1)  // Won't compile
  Eq[String].eqv("1", "2")
  
  /*
   * Importing 'syntax' allows us to write a.===(b) by bringing
   * an implicit conversion into scope: 
   * EqSyntax(a)(implicit ev: Eq[A]).===(b: A)
   */
  import cats.syntax.eq._
  "fish" === "cake"  // false
 
  /*
   * To compare custom types we must create a type class instance
   * for Eq.  This is the analogue of writing a custom comparison 
   * equality method, but more flexible. 
   */
  case class TimeStamped(number: Int, time: Long = System.nanoTime)
  object TimeStamped{
    // Lets ignore the time stamp and just compare the numbers
	  implicit val eqInstance: Eq[TimeStamped] = 
	    Eq.instance[TimeStamped]{(a,b) =>
	      a.number === b.number
	  }
  }
  
  val a = TimeStamped(2) 
  val b = TimeStamped(1 + 1)
  println("a == b: "+(a == b))    // false
  println("a === b: "+(a === b))  // true
  
  
  /*
   * Challenge
   * Complete the TODO so the assertions pass
   */
  case class Box[A](label: String, item: A)
  object Box{
    // TODO create Eq type class instance here for Box here
  }
 
  
  // These should pass when uncommented
//  assert(Box("Food", "Chocolate") =!= Box("Food", "Cake"), "Chocolate is different to cake")
//  assert(Box("TS", TimeStamped(1)) === Box("TS", TimeStamped(1)), "Timestamps in boxes are 'equivalent'")
  
  //This shouldn't compile
//  Box("One", 1) =!= Box("One", "1")
  
  
  
  
}