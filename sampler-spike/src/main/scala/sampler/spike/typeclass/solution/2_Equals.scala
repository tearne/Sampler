package sampler.spike.typeclass.solution

import cats.Eq
import cats.std.all._
import cats.syntax.eq._

object Equals {
  case class TimeStamped(number: Int, time: Long = System.nanoTime)
  object TimeStamped{
    // Lets ignore the time stamp and just compare the numbers
	  implicit val eqInstance: Eq[TimeStamped] = 
	    Eq.instance[TimeStamped]{(a,b) =>
	      a.number === b.number
	  }
  }
  
  /*
   * Challenge
   * Complete the TODO so the assertions pass
   */
  case class Box[A](label: String, item: A)
  object Box{
    // TODO create Eq type class instance for Box here
    implicit def eqInstance[A: Eq]: Eq[Box[A]] =
      Eq.instance[Box[A]] { (a,b) =>
        a.label === b.label && a.item === b.item
      }
  }
  
  // These should pass
  assert(Box("Food", "Chocolate") =!= Box("Food", "Cake"), "Chocolate is different to cake")
  assert(Box("TS", TimeStamped(1)) === Box("TS", TimeStamped(1)), "Timestamps in boxes are 'equivalent'")
  
  //This shouldn't compile
//  Box("One", 1) =!= Box("One", "1")
}