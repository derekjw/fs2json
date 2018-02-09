package fs2json

import scala.language.higherKinds

sealed trait JsonToken {
  def value: String
}

case object ObjectStart extends JsonToken { val value = "{"}
case object ObjectEnd extends JsonToken { val value = "}"}

case class ObjectField(value: String) extends JsonToken

case object ArrayStart extends JsonToken { val value = "["}
case object ArrayEnd extends JsonToken { val value = "]"}

// TODO: add decode methods for string and number
case class JsonString(value: String) extends JsonToken
case class JsonNumber(value: String) extends JsonToken
case object JsonTrue extends JsonToken { val value = "true"}
case object JsonFalse extends JsonToken { val value = "false"}
case object JsonNull extends JsonToken { val value = "null"}

case class TokenParserFailure(message: String, cause: Option[Throwable] = None) extends RuntimeException(message, cause.orNull)

sealed trait JsonStyle

object JsonStyle {
  case object NoSpaces extends JsonStyle
  case object Pretty extends JsonStyle
  case class SemiPretty(levelLimit: Int) extends JsonStyle
}
