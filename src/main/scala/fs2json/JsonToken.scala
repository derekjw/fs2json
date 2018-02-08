package fs2json

import cats.effect.IO
import fs2._

import scala.language.higherKinds

sealed trait JsonToken

case object ObjectStart extends JsonToken
case object ObjectEnd extends JsonToken

case class ObjectField(value: String) extends JsonToken

case object ArrayStart extends JsonToken
case object ArrayEnd extends JsonToken

case class JsonString(value: String) extends JsonToken
case class JsonNumber(value: String) extends JsonToken
case object JsonTrue extends JsonToken
case object JsonFalse extends JsonToken
case object JsonNull extends JsonToken

case class TokenParserFailure(message: String, cause: Option[Throwable] = None) extends RuntimeException(message, cause.orNull)

object Test extends StreamApp[IO] {
  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, StreamApp.ExitCode] = {
    val jsonString = """
      [
      true,
      false,
      null,
      {},
      ["Hello world!", "World\nHello!"]
       {
       "foo": "bar",
       "baz":  {"1" :   -2.1234, "3": "4"}
      },
      ]
    """

    Stream.emit(jsonString)
      .through(text.utf8Encode)
      .through(tokenParser)
      .evalMap { token =>
        IO(println(token))
      }
      .drain ++ Stream.emit(StreamApp.ExitCode.Success)
  }
}