package sampler.spike.typeclass

object Orderings extends App {
   
   /*
    *  We can sort list of Ints and Strings
    */
   println(List(3,5,1,4,2).sorted)
   println(List("Bee", "Cat", "Ant").sorted)
   
   /*
    *  What about a list of custom data types? ...
    */
   case class Data(value: Int)
   val dataList: List[Data] = List(3,5,1,4,2).map(Data.apply)
   
//   println(dataList.sorted)  // Uncomment for compile-time error:
   
   /*
    * What manner of whichcraft allows List[Int] & List[String] to 
    * have '.sorted' called but not List[Data]?
    * 
    * Look at the API for a clue
    * http://www.scala-lang.org/api/current/#scala.collection.immutable.List
    * 
    * def sorted[B >: A](implicit ord: Ordering[B]): List[A] 
    * 
    * ...it's the implicit Ordering
    * 
    *  If we'd like to be able to sort List[Data], we need to provide
    *  an ordering.  
    */
   
   println(
     dataList.sorted(Ordering.fromLessThan[Data]((a,b) => a.value < b.value))
   )
   
   /*
    * We can put an implicit for the ordering in the companion object for
    * Data.  The compiler will know to look there since we are dealing
    * with a List[Data]
    */
   
   object Data{
     implicit val orderingInstance =  
       Ordering.fromLessThan[Data]((a,b) => a.value < b.value)
   }
   
   println(dataList.sorted)  // Much nicer
   
   
   /*
    * Here's another custom data type which can't be '.sorted' by default
    */
   sealed trait Item
   final case object Coffee extends Item
   final case class Thing(thing: String)  extends Item

   /*
    *  Challenge:
    *  Complete the customOrdering so that the code below prints out 
    *  List(Coffee, Coffee, Thing(A), Thing(B), Thing(C))
    */
   implicit val customOrdering: Ordering[Item] = ???
   
   val items = List[Item](Coffee, Thing("C"), Coffee, Thing("A"), Thing("B"))
   println(items.sorted)  // Uncomment me
   
   println("cat" < "dog")
}