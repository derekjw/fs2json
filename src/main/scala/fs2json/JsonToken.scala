package fs2json

import fs2.Chunk

import scala.language.higherKinds

sealed trait JsonToken extends Product {
  def value: Chunk.Bytes

  override def toString: String = s"$productPrefix(${new String(value.toArray)})"
}

case object ObjectStart extends JsonToken { val value = Chunk.Bytes(Array[Byte]('{')) }
case object ObjectEnd extends JsonToken { val value = Chunk.Bytes(Array[Byte]('}'))}

case class ObjectField(value: Chunk.Bytes) extends JsonToken
object ObjectField {
  def fromString(str: String) = ObjectField(Chunk.Bytes(s""""$str"""".getBytes("UTF-8")))
}

case object ArrayStart extends JsonToken { val value = Chunk.Bytes(Array[Byte]('['))}
case object ArrayEnd extends JsonToken { val value = Chunk.Bytes(Array[Byte](']'))}

// TODO: add decode methods for string and number
case class JsonString(value: Chunk.Bytes) extends JsonToken
case class JsonNumber(value: Chunk.Bytes) extends JsonToken
case object JsonTrue extends JsonToken { val value = Chunk.Bytes(Array[Byte]('t', 'r', 'u', 'e')) }
case object JsonFalse extends JsonToken { val value = Chunk.Bytes(Array[Byte]('f', 'a', 'l', 's', 'e')) }
case object JsonNull extends JsonToken { val value = Chunk.Bytes(Array[Byte]('n', 'u', 'l', 'l')) }

case class TokenParserFailure(message: String, cause: Option[Throwable] = None) extends RuntimeException(message, cause.orNull)

sealed trait JsonStyle

object JsonStyle {
  case object NoSpaces extends JsonStyle
  case object Pretty extends JsonStyle
  case class SemiPretty(levelLimit: Int) extends JsonStyle
}
