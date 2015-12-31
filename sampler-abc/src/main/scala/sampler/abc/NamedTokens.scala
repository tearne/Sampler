package sampler.abc

import play.api.libs.json.JsValue
import play.api.libs.json.JsNumber

trait Tokenable[T]{
	def namedTokens(data: T): NamedTokens
}

case class NamedTokens(toks: (String, NamedTokens.Token)*) {
	def toMap = toks.toMap
	
	def ++(moreTokens: (String, NamedTokens.Token)*) = NamedTokens(moreTokens ++ toks: _*)
	def +(anotherToken: (String, NamedTokens.Token)) = NamedTokens(anotherToken +: toks: _*)
}
object NamedTokens {
	import scala.language.implicitConversions
	
	trait Token{
		def get(): JsValue
	}
	case class BigDecimalToken(bd: BigDecimal) extends Token {
		def get = JsNumber(bd)
	}
	
	implicit def toToken(d: Double): BigDecimalToken = toToken(BigDecimal(d))
	implicit def toToken(bd: BigDecimal): BigDecimalToken = BigDecimalToken(bd)
	
	def named(namedTokens: (String, Token)*): NamedTokens = NamedTokens(namedTokens: _*)
	
}