package fs2json

import fs2.{Chunk, Pipe, Pull, Segment}
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
          Pull.output(jsons.flatMap(jsonToTokens).mapResult(_ => ())).as(Some(rest))
        case None =>
          Pull.pure(None)
      }
    }

  def jsonToTokens(json: Json): Segment[JsonToken, Unit] =
    json.fold(
      Segment.singleton(JsonNull),
      b => Segment.singleton(if (b) JsonTrue else JsonFalse),
      n => Segment.singleton(JsonNumber(Chunk.Bytes(n.asJson.noSpaces.getBytes("UTF-8")))),
      s => Segment.singleton(JsonString(Chunk.Bytes(s.asJson.noSpaces.getBytes("UTF-8")))),
      a => Segment.singleton(ArrayStart) ++ Segment.seq(a).flatMap(jsonToTokens).mapResult(_ => ()) ++ Segment.singleton(ArrayEnd),
      o => Segment.singleton(ObjectStart) ++
        Segment.seq(o.toVector).flatMap(kv =>
          Segment.singleton(ObjectField(Chunk.Bytes(Json.fromString(kv._1).noSpaces.getBytes("UTF-8")))) ++ jsonToTokens(kv._2)).mapResult(_ => ()) ++
        Segment.singleton(ObjectEnd))

}
