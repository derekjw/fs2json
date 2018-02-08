package fs2json

import cats.effect.IO
import fs2._

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

object Test extends StreamApp[IO] {
  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, StreamApp.ExitCode] = {
    val jsonString = """
      [
      true,
      false,
      null,
      {},
      ["Hello world!", "World\n\"Hello\"!"]
       {
       "foo": "bar",
       "baz":  {"1" :   -2.1234, "3": "4"}
      },
      ]
    """

    Stream.emit(jsonString)
      .through(text.utf8Encode)
      .through(tokenParser)
      .segments
      .evalMap { token =>
        IO(println(token))
      }
      .drain ++ Stream.emit(StreamApp.ExitCode.Success)
  }
}