package fs2json

package object circe {
  import fs2.Pipe
  import io.circe.Json
  import io.circe.jawn.CirceSupportParser.facade

  import scala.language.higherKinds

  def valueStream[F[_]]: Pipe[F, JsonToken, Json] = jawn.valueStream[F, Json]

}
