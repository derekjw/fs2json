package fs2json

import fs2._

import scala.annotation.tailrec
import scala.language.higherKinds

trait TokenFilter[F[_]] extends Pipe[F, JsonToken, JsonToken]

object TokenFilter extends TokenFilterBuilder {
  val targetPath: List[Direction] = Nil

  sealed trait Direction
  case object DownArray extends Direction
  case object DownObject extends Direction
  case class DownField(objectField: ObjectField) extends Direction

}

trait ObjectTokenFilterBuilder { parent =>
  def targetPath: List[TokenFilter.Direction]

  def downField(objectField: ObjectField): TokenFilterBuilder = new TokenFilterBuilder {
    override val targetPath: List[TokenFilter.Direction] = TokenFilter.DownField(objectField) :: parent.targetPath
  }
  def downField(fieldName: String): TokenFilterBuilder = downField(ObjectField.fromString(fieldName))

  def removeField[F[_]](fieldName: String): TokenFilter[F] = removeField(ObjectField.fromString(fieldName))
  def removeField[F[_]](objectField: ObjectField): TokenFilter[F] = {
    val targetDirection = TokenFilter.DownField(objectField) :: parent.targetPath

    case class State(dropRanges: Vector[(Int, Int)], path: List[TokenFilter.Direction], toTargetPath: List[TokenFilter.Direction], fromTargetPath: List[TokenFilter.Direction])

    @tailrec
    def findDropRanges(chunk: Chunk[JsonToken], pos: Int, dropPos: Int, state: State): State =
      if (pos >= chunk.size) {
        state
      } else {
        chunk(pos) match {
          case objectField: ObjectField =>
            state match {
              case State(dropRanges, Nil, (hd @ TokenFilter.DownField(`objectField`)) :: Nil, fromTargetPath) => // begin dropping
                findDropRanges(chunk, pos + 1, pos, State(dropRanges, Nil, Nil, hd :: fromTargetPath))
              case State(_, Nil, (hd @ TokenFilter.DownField(`objectField`)) :: toTargetPath, fromTargetPath) => // going down towards target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTargetPath = toTargetPath, fromTargetPath = hd :: fromTargetPath))
              case _ => // going down other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(path = TokenFilter.DownField(objectField) :: state.path))
            }
          case ObjectEnd =>
            state match {
              case State(dropRanges, TokenFilter.DownObject :: Nil, Nil, hd :: tl) => // done dropping
                findDropRanges(chunk, pos + 1, 0, State(dropRanges :+ (dropPos, pos + 1), Nil, hd :: Nil, tl))
              case State(_, Nil, toTargetPath, TokenFilter.DownObject :: (hd @ TokenFilter.DownField(_)) :: fromTargetPath) => // going back up from target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTargetPath = hd :: TokenFilter.DownObject :: toTargetPath, fromTargetPath = fromTargetPath))
              case State(_, Nil, toTargetPath, TokenFilter.DownObject :: fromTargetPath) => // going back up from target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTargetPath = TokenFilter.DownObject :: toTargetPath, fromTargetPath = fromTargetPath))
              case State(_, TokenFilter.DownObject :: TokenFilter.DownField(_) :: tl, _, _) => // going back up
                findDropRanges(chunk, pos + 1, dropPos, state.copy(path = tl))
              case State(_, TokenFilter.DownObject :: tl, _, _) => // going back up
                findDropRanges(chunk, pos + 1, dropPos, state.copy(path = tl))
              case _ => // TODO: better failure handling of bad json
                throw new RuntimeException("ruh roh")
            }
          case ObjectStart =>
            state match {
              case State(_, Nil, TokenFilter.DownObject :: toTargetPath, fromTargetPath) => // going down towards target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTargetPath = toTargetPath, fromTargetPath = TokenFilter.DownObject :: fromTargetPath))
              case _ => // going down other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(path = TokenFilter.DownObject :: state.path))
            }
          case ArrayEnd =>
            state match {
              case State(dropRanges, TokenFilter.DownArray :: Nil, Nil, hd :: tl) => // done dropping
                findDropRanges(chunk, pos + 1, 0, State(dropRanges :+ (dropPos, pos + 1), Nil, hd :: Nil, tl))
              case State(_, Nil, toTargetPath, TokenFilter.DownArray :: (hd @ TokenFilter.DownField(_)) :: fromTargetPath) => // going back up from target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTargetPath = hd :: TokenFilter.DownArray :: toTargetPath, fromTargetPath = fromTargetPath))
              case State(_, Nil, toTargetPath, TokenFilter.DownArray :: fromTargetPath) => // going back up from target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTargetPath = TokenFilter.DownArray :: toTargetPath, fromTargetPath = fromTargetPath))
              case State(_, TokenFilter.DownArray :: TokenFilter.DownField(_) :: tl, _, _) => // going back up from other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(path = tl))
              case State(_, TokenFilter.DownArray :: tl, _, _) => // going back up from other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(path = tl))
              case _ => // TODO: better failure handling of bad json
                throw new RuntimeException("ruh roh")
            }
          case ArrayStart =>
            state match {
              case State(_, Nil, TokenFilter.DownArray :: toTargetPath, fromTargetPath) => // going down towards target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTargetPath = toTargetPath, fromTargetPath = TokenFilter.DownArray :: fromTargetPath))
              case _ => // going down other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(path = TokenFilter.DownArray :: state.path))
            }
          case JsonNull | JsonTrue | JsonFalse | _: JsonNumber | _: JsonString =>
            state match {
              case State(dropRanges, Nil, Nil, hd :: tl) => // done dropping
                findDropRanges(chunk, pos + 1, 0, State(dropRanges :+ (dropPos, pos + 1), Nil, hd :: Nil, tl))
              case State(_, TokenFilter.DownField(_) :: path, _, _) =>
                findDropRanges(chunk, pos + 1, dropPos, state.copy(path = path))
              case _ =>
                findDropRanges(chunk, pos + 1, dropPos, state)
            }
        }
      }

    def next(stream: Stream[F, JsonToken], path: List[TokenFilter.Direction], toTargetPath: List[TokenFilter.Direction], fromTargetPath: List[TokenFilter.Direction]): Pull[F, JsonToken, Unit] =
      stream.pull.unconsChunk.flatMap {
        case Some((jsonTokens, rest)) =>
          val state = findDropRanges(jsonTokens, 0, 0, State(Vector.empty, path, toTargetPath, fromTargetPath))
          val output = if (state.dropRanges.isEmpty) {
            Pull.outputChunk(jsonTokens)
          } else {
            val (_, lastChunk, result) = state.dropRanges.foldLeft((0, jsonTokens, Segment.empty[JsonToken])) {
              case ((pos, remaining, acc), (dropStart, dropEnd)) =>
                (dropEnd, remaining.drop(dropEnd - pos), acc ++ Segment.chunk(remaining.take(dropStart - pos)))
            }
            Pull.output(result ++ Segment.chunk(lastChunk))
          }
          output >> next(rest, state.path, state.toTargetPath, state.fromTargetPath)
        case None => Pull.done
      }

    next(_, Nil, targetDirection.reverse, Nil).stream
  }
}

trait TokenFilterBuilder { parent =>
  def targetPath: List[TokenFilter.Direction]

  def downArray: TokenFilterBuilder = new TokenFilterBuilder {
    override val targetPath: List[TokenFilter.Direction] = TokenFilter.DownArray :: parent.targetPath
  }
  def downObject: ObjectTokenFilterBuilder = new ObjectTokenFilterBuilder {
    override val targetPath: List[TokenFilter.Direction] = TokenFilter.DownObject :: parent.targetPath
  }

}
