package fs2json

import fs2.Chunk

import scala.annotation.switch
import scala.collection.mutable
import scala.language.higherKinds

sealed trait JsonToken extends Product with Serializable {
  def value: Chunk.Bytes

  override def toString: String = s"$productPrefix(${new String(value.toArray)})"
}

case object ObjectStart extends JsonToken { val value = Chunk.Bytes(Array[Byte]('{')) }
case object ObjectEnd extends JsonToken { val value = Chunk.Bytes(Array[Byte]('}')) }

case class ObjectField(value: Chunk.Bytes) extends JsonToken
object ObjectField {
  def fromString(str: String) = ObjectField(JsonString.fromString(str).value)
}

case object ArrayStart extends JsonToken { val value = Chunk.Bytes(Array[Byte]('[')) }
case object ArrayEnd extends JsonToken { val value = Chunk.Bytes(Array[Byte](']')) }

// TODO: add decode methods for string and number
case class JsonString(value: Chunk.Bytes) extends JsonToken
object JsonString {
  def fromString(value: String): JsonString = {
    // Based on circe printer code
    val builder = new mutable.ArrayBuilder.ofByte

    builder += '"'

    var i = 0

    while (i < value.length) {
      val c = value.charAt(i)

      if ((c == '"' || c == '\\' || c == '\b' || c == '\f' || c == '\n' || c == '\r' || c == '\t') || Character.isISOControl(c) || c.toInt > 127) {
        builder += '\\'
        (c: @switch) match {
          case '"'  => builder += '"'
          case '\\' => builder += '\\'
          case '\b' => builder += 'b'
          case '\f' => builder += 'f'
          case '\n' => builder += 'n'
          case '\r' => builder += 'r'
          case '\t' => builder += 't'
          case control =>
            String.format("u%04x", Integer.valueOf(control.toInt)).foreach(builder += _.toByte)
        }
      } else {
        builder += c.toByte
      }

      i += 1
    }

    builder += '"'

    JsonString(Chunk.Bytes(builder.result()))
  }
}

case class JsonNumber(value: Chunk.Bytes) extends JsonToken
object JsonNumber {
  def fromString(str: String) = JsonString(Chunk.Bytes(str.getBytes("UTF-8")))
}

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
