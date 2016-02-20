package sampler.spike.typeclass


object Writing extends App {
  import cats._
  import cats.syntax.show._
  import cats.std.all._
  
  println("Hello".show)
 
  case class Data[T](value: T)
  object Data{
    implicit def showInstance[T: Show]: Show[Data[T]] = 
      Show.show[Data[T]](data => "Data = "+data.value.show)
  }
  println(Data("kittens!").show)
  println(Data("kittens!").toString)
  
  import play.api.libs.json._
  Json.toJson()
}