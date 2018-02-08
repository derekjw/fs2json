package fs2json

import scala.language.higherKinds

sealed trait JsonToken

case object ObjectStart extends JsonToken { override val toString = "{"}
case object ObjectEnd extends JsonToken { override val toString = "}"}

case class ObjectField(value: String) extends JsonToken { override def toString = s""""$value":"""}

case object ArrayStart extends JsonToken { override val toString = "["}
case object ArrayEnd extends JsonToken { override val toString = "]"}

case class JsonString(value: String) extends JsonToken { override def toString = s""""$value""""}
case class JsonNumber(value: String) extends JsonToken { override val toString: String = value}
case object JsonTrue extends JsonToken { override val toString = "true"}
case object JsonFalse extends JsonToken { override val toString = "false"}
case object JsonNull extends JsonToken { override val toString = "null"}

case class TokenParserFailure(message: String, cause: Option[Throwable] = None) extends RuntimeException(message, cause.orNull)
