package fs2json

import fs2.{Chunk, Pipe, Pull}
import io.circe.Json
import io.circe.jawn.CirceSupportParser.facade
import io.circe.syntax._

import scala.language.higherKinds

package object circe {

  def valueStream[F[_]]: Pipe[F, JsonToken, Json] = jawn.valueStream[F, Json]

  def tokenStream[F[_]]: Pipe[F, Json, JsonToken] =
    _.repeatPull {
      _.uncons.flatMap {
        case Some((jsons, rest)) =>
          Pull.output(jsons.flatMap(jsonToTokens)).as(Some(rest))
        case None =>
          Pull.pure(None)
      }
    }

  // FIXME: Better chunk concat
  def jsonToTokens(json: Json): Chunk[JsonToken] =
    json.fold(
      Chunk.singleton(JsonNull),
      b => Chunk.singleton(if (b) JsonTrue else JsonFalse),
      n => Chunk.singleton(JsonNumber(Chunk.Bytes(n.asJson.noSpaces.getBytes("UTF-8")))),
      s => Chunk.singleton(JsonString(Chunk.Bytes(s.asJson.noSpaces.getBytes("UTF-8")))),
      a => Chunk.concat(List(Chunk.singleton(ArrayStart), Chunk.seq(a).flatMap(jsonToTokens), Chunk.singleton(ArrayEnd))),
      o =>
        Chunk.concat(
          List(
            Chunk.singleton(ObjectStart),
            Chunk.seq(o.toVector).flatMap(kv => Chunk.concat(List(Chunk.singleton(ObjectField(Chunk.Bytes(Json.fromString(kv._1).noSpaces.getBytes("UTF-8")))), jsonToTokens(kv._2)))),
            Chunk.singleton(ObjectEnd)
          ))
    )

}
