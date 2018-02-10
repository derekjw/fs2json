package fs2json

import fs2._

import scala.annotation.tailrec
import scala.language.higherKinds

trait TokenFilter[F[_]] extends Pipe[F, JsonToken, JsonToken]

object TokenFilter extends TokenFilterBuilder {
  val targetDirection: List[Direction] = Nil

  sealed trait Direction
  case object DownArray extends Direction
  case object DownObject extends Direction
  case class DownField(objectField: ObjectField) extends Direction

}

trait ObjectTokenFilterBuilder { parent =>
  def targetDirection: List[TokenFilter.Direction]

  def downField(objectField: ObjectField): TokenFilterBuilder = new TokenFilterBuilder {
    override val targetDirection: List[TokenFilter.Direction] = TokenFilter.DownField(objectField) :: parent.targetDirection
  }
  def downField(fieldName: String): TokenFilterBuilder = downField(ObjectField.fromString(fieldName))

  def removeField[F[_]](fieldName: String): TokenFilter[F] = removeField(ObjectField.fromString(fieldName))
  def removeField[F[_]](objectField: ObjectField): TokenFilter[F] = {
    val targetDirection = TokenFilter.DownField(objectField) :: parent.targetDirection

    case class State(dropRanges: Vector[(Int, Int)], currentDirection: List[TokenFilter.Direction], toTargetDirection: List[TokenFilter.Direction], fromTargetDirection: List[TokenFilter.Direction])

    @tailrec
    def findDropRanges(chunk: Chunk[JsonToken], pos: Int, dropPos: Int, state: State): State =
      if (pos >= chunk.size) {
        state
      } else {
        chunk(pos) match {
          case objectField: ObjectField =>
            state match {
              case State(dropRanges, Nil, (hd @ TokenFilter.DownField(`objectField`)) :: Nil, fromTargetDirection) => // begin dropping
                findDropRanges(chunk, pos + 1, pos, State(dropRanges, Nil, Nil, hd :: fromTargetDirection))
              case State(_, Nil, (hd @ TokenFilter.DownField(`objectField`)) :: toTargetDirection, fromTargetDirection) => // going down towards target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTargetDirection = toTargetDirection, fromTargetDirection = hd :: fromTargetDirection))
              case _ => // going down other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(currentDirection = TokenFilter.DownField(objectField) :: state.currentDirection))
            }
          case ObjectEnd =>
            state match {
              case State(dropRanges, TokenFilter.DownObject :: Nil, Nil, hd :: tl) => // done dropping
                findDropRanges(chunk, pos + 1, 0, State(dropRanges :+ (dropPos, pos + 1), Nil, hd :: Nil, tl))
              case State(_, Nil, toTargetDirection, TokenFilter.DownObject :: (hd @ TokenFilter.DownField(_)) :: fromTargetDirection) => // going back up from target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTargetDirection = hd :: TokenFilter.DownObject :: toTargetDirection, fromTargetDirection = fromTargetDirection))
              case State(_, Nil, toTargetDirection, TokenFilter.DownObject :: fromTargetDirection) => // going back up from target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTargetDirection = TokenFilter.DownObject :: toTargetDirection, fromTargetDirection = fromTargetDirection))
              case State(_, TokenFilter.DownObject :: TokenFilter.DownField(_) :: tl, _, _) => // going back up
                findDropRanges(chunk, pos + 1, dropPos, state.copy(currentDirection = tl))
              case State(_, TokenFilter.DownObject :: tl, _, _) => // going back up
                findDropRanges(chunk, pos + 1, dropPos, state.copy(currentDirection = tl))
              case _ => // TODO: better failure handling of bad json
                throw new RuntimeException("ruh roh")
            }
          case ObjectStart =>
            state match {
              case State(_, Nil, TokenFilter.DownObject :: toTargetDirection, fromTargetDirection) => // going down towards target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTargetDirection = toTargetDirection, fromTargetDirection = TokenFilter.DownObject :: fromTargetDirection))
              case _ => // going down other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(currentDirection = TokenFilter.DownObject :: state.currentDirection))
            }
          case ArrayEnd =>
            state match {
              case State(dropRanges, TokenFilter.DownArray :: Nil, Nil, hd :: tl) => // done dropping
                findDropRanges(chunk, pos + 1, 0, State(dropRanges :+ (dropPos, pos + 1), Nil, hd :: Nil, tl))
              case State(_, Nil, toTargetDirection, TokenFilter.DownArray :: (hd @ TokenFilter.DownField(_)) :: fromTargetDirection) => // going back up from target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTargetDirection = hd :: TokenFilter.DownArray :: toTargetDirection, fromTargetDirection = fromTargetDirection))
              case State(_, Nil, toTargetDirection, TokenFilter.DownArray :: fromTargetDirection) => // going back up from target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTargetDirection = TokenFilter.DownArray :: toTargetDirection, fromTargetDirection = fromTargetDirection))
              case State(_, TokenFilter.DownArray :: TokenFilter.DownField(_) :: tl, _, _) => // going back up from other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(currentDirection = tl))
              case State(_, TokenFilter.DownArray :: tl, _, _) => // going back up from other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(currentDirection = tl))
              case _ => // TODO: better failure handling of bad json
                throw new RuntimeException("ruh roh")
            }
          case ArrayStart =>
            state match {
              case State(_, Nil, TokenFilter.DownArray :: toTargetDirection, fromTargetDirection) => // going down towards target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTargetDirection = toTargetDirection, fromTargetDirection = TokenFilter.DownArray :: fromTargetDirection))
              case _ => // going down other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(currentDirection = TokenFilter.DownArray :: state.currentDirection))
            }
          case JsonNull | JsonTrue | JsonFalse | _: JsonNumber | _: JsonString =>
            state match {
              case State(dropRanges, Nil, Nil, hd :: tl) => // done dropping
                findDropRanges(chunk, pos + 1, 0, State(dropRanges :+ (dropPos, pos + 1), Nil, hd :: Nil, tl))
              case State(_, TokenFilter.DownField(_) :: currentDirection, _, _) =>
                findDropRanges(chunk, pos + 1, dropPos, state.copy(currentDirection = currentDirection))
              case _ =>
                findDropRanges(chunk, pos + 1, dropPos, state)
            }
        }
      }

    def next(stream: Stream[F, JsonToken], currentDirection: List[TokenFilter.Direction], toTargetDirection: List[TokenFilter.Direction], fromTargetDirection: List[TokenFilter.Direction]): Pull[F, JsonToken, Unit] =
      stream.pull.unconsChunk.flatMap {
        case Some((jsonTokens, rest)) =>
          val state = findDropRanges(jsonTokens, 0, 0, State(Vector.empty, currentDirection, toTargetDirection, fromTargetDirection))
          val output = if (state.dropRanges.isEmpty) {
            Pull.outputChunk(jsonTokens)
          } else {
            val (_, lastChunk, result) = state.dropRanges.foldLeft((0, jsonTokens, Segment.empty[JsonToken])) {
              case ((pos, remaining, acc), (dropStart, dropEnd)) =>
                (dropEnd, remaining.drop(dropEnd - pos), acc ++ Segment.chunk(remaining.take(dropStart - pos)))
            }
            Pull.output(result ++ Segment.chunk(lastChunk))
          }
          output >> next(rest, state.currentDirection, state.toTargetDirection, state.fromTargetDirection)
        case None => Pull.done
      }

    next(_, Nil, targetDirection.reverse, Nil).stream
  }
}

trait TokenFilterBuilder { parent =>
  def targetDirection: List[TokenFilter.Direction]

  def downArray: TokenFilterBuilder = new TokenFilterBuilder {
    override val targetDirection: List[TokenFilter.Direction] = TokenFilter.DownArray :: parent.targetDirection
  }
  def downObject: ObjectTokenFilterBuilder = new ObjectTokenFilterBuilder {
    override val targetDirection: List[TokenFilter.Direction] = TokenFilter.DownObject :: parent.targetDirection
  }

}
