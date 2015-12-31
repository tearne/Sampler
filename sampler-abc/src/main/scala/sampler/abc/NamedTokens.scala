package sampler.abc

import play.api.libs.json.JsValue
import play.api.libs.json.JsNumber

trait Tokenable[T]{
	def namedTokens(data: T): NamedTokens
}

case class NamedTokens(toks: (String, JsValue)*) {
	def toMap = toks.toMap
	
	def ++(moreTokens: (String, JsValue)*) = NamedTokens(moreTokens ++ toks: _*)
	def +(anotherToken: (String, JsValue)) = NamedTokens(anotherToken +: toks: _*)
}
object NamedTokens {
	import scala.language.implicitConversions
	
	def named(namedTokens: (String, JsValue)*): NamedTokens = NamedTokens(namedTokens: _*)
}