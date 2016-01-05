package sampler.io

import play.api.libs.json.JsValue
import play.api.libs.json.Writes
import scala.language.implicitConversions

trait Tokenable[T]{
	def getTokens(data: T): Tokens
}

case class Tokens(map: Map[String, JsValue]) {
	def ++(moreTokens: (String, JsValue)*) = Tokens(map ++ moreTokens)
	def +(anotherToken: (String, JsValue)) = Tokens(map + anotherToken)
	def +(that: Tokens) = Tokens(map ++ that.map)
}
object Tokens {
	import scala.language.implicitConversions
	
	case class WrappedJsValue(jsValue: JsValue)
	
	implicit def tokener[T](field: T)(implicit writes: Writes[T]): WrappedJsValue = 
		WrappedJsValue(writes.writes(field))
	
	def named(tokens: (String, WrappedJsValue)*): Tokens = Tokens(tokens.toMap.mapValues(_.jsValue))
}