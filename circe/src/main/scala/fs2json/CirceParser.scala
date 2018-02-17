package fs2json

import fs2.Pipe
import io.circe.Json
import io.circe.jawn.CirceSupportParser.facade

import scala.language.higherKinds

object CirceParser {
  def valueStream[F[_]]: Pipe[F, JsonToken, Json] = JawnParser.valueStream[F, Json]
}
