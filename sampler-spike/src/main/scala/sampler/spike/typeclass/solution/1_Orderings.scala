package sampler.spike.typeclass.solution

object Orderings {
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
   implicit val customOrdering: Ordering[Item] = {
     Ordering.fromLessThan[Item]{
         case (Coffee, Coffee) => true
         case (Coffee, p: Thing) => true
         case (p: Thing, Coffee) => false
         case (Thing(strA), Thing(strB)) => strA < strB
     }
   }
   
   val items = List[Item](Coffee, Thing("C"), Coffee, Thing("A"), Thing("B"))
   println(items.sorted)  // Uncomment me
}