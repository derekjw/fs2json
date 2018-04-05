package fs2json

import fs2._

import scala.annotation.tailrec
import scala.language.higherKinds

object TokenFilter extends TokenFilterBuilder {
  val target: List[Direction] = Nil

  sealed trait Direction
  case object DownArray extends Direction
  case object DownObject extends Direction
  case class DownField(objectField: ObjectField) extends Direction
}

trait ObjectTokenFilterBuilder { parent =>
  def target: List[TokenFilter.Direction]

  def downField(objectField: ObjectField): TokenFilterBuilder = new TokenFilterBuilder {
    override val target: List[TokenFilter.Direction] = TokenFilter.DownField(objectField) :: parent.target
  }
  def downField(fieldName: String): TokenFilterBuilder = downField(ObjectField.fromString(fieldName))

  def removeField[F[_]](fieldName: String): Pipe[F, JsonToken, JsonToken] = removeFields(Set(fieldName))
  def removeFields[F[_]](fieldNames: Iterable[String]): Pipe[F, JsonToken, JsonToken] = {
    val targetFields = fieldNames.map(ObjectField.fromString).toSet
    val target = parent.target

    case class State(dropping: Boolean, dropRanges: Vector[(Int, Int)], offTarget: List[TokenFilter.Direction], toTarget: List[TokenFilter.Direction], onTarget: List[TokenFilter.Direction])

    @tailrec
    def findDropRanges(chunk: Chunk[JsonToken], pos: Int, dropPos: Int, state: State): State =
      if (pos >= chunk.size) {
        if (state.dropping) {
          state.copy(dropRanges = state.dropRanges :+ (dropPos, pos))
        } else {
          state
        }
      } else {
        chunk(pos) match {
          case objectField: ObjectField =>
            state match {
              case State(_, dropRanges, Nil, Nil, onTarget) if targetFields(objectField) => // begin dropping
                findDropRanges(chunk, pos + 1, pos, State(dropping = true, dropRanges, Nil, Nil, onTarget))
              case State(_, _, Nil, (hd @ TokenFilter.DownField(`objectField`)) :: toTarget, onTarget) => // going down towards target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTarget = toTarget, onTarget = hd :: onTarget))
              case _ => // going down other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(offTarget = TokenFilter.DownField(objectField) :: state.offTarget))
            }
          case ObjectEnd =>
            state match {
              case State(_, dropRanges, TokenFilter.DownObject :: Nil, Nil, onTarget) => // done dropping
                findDropRanges(chunk, pos + 1, 0, State(dropping = false, dropRanges :+ (dropPos, pos + 1), Nil, Nil, onTarget))
              case State(_, _, Nil, toTarget, TokenFilter.DownObject :: (hd @ TokenFilter.DownField(_)) :: onTarget) => // going back up from target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTarget = hd :: TokenFilter.DownObject :: toTarget, onTarget = onTarget))
              case State(_, _, Nil, toTarget, TokenFilter.DownObject :: onTarget) => // going back up from target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTarget = TokenFilter.DownObject :: toTarget, onTarget = onTarget))
              case State(_, _, TokenFilter.DownObject :: TokenFilter.DownField(_) :: offTarget, _, _) => // going back up
                findDropRanges(chunk, pos + 1, dropPos, state.copy(offTarget = offTarget))
              case State(_, _, TokenFilter.DownObject :: offTarget, _, _) => // going back up
                findDropRanges(chunk, pos + 1, dropPos, state.copy(offTarget = offTarget))
              case _ => // TODO: better failure handling of bad json
                throw new RuntimeException("ruh roh")
            }
          case ObjectStart =>
            state match {
              case State(_, _, Nil, TokenFilter.DownObject :: toTarget, onTarget) => // going down towards target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTarget = toTarget, onTarget = TokenFilter.DownObject :: onTarget))
              case _ => // going down other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(offTarget = TokenFilter.DownObject :: state.offTarget))
            }
          case ArrayEnd =>
            state match {
              case State(_, dropRanges, TokenFilter.DownArray :: Nil, Nil, onTarget) => // done dropping
                findDropRanges(chunk, pos + 1, 0, State(dropping = false, dropRanges :+ (dropPos, pos + 1), Nil, Nil, onTarget))
              case State(_, _, Nil, toTarget, TokenFilter.DownArray :: (hd @ TokenFilter.DownField(_)) :: onTarget) => // going back up from target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTarget = hd :: TokenFilter.DownArray :: toTarget, onTarget = onTarget))
              case State(_, _, Nil, toTarget, TokenFilter.DownArray :: onTarget) => // going back up from target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTarget = TokenFilter.DownArray :: toTarget, onTarget = onTarget))
              case State(_, _, TokenFilter.DownArray :: TokenFilter.DownField(_) :: offTarget, _, _) => // going back up from other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(offTarget = offTarget))
              case State(_, _, TokenFilter.DownArray :: offTarget, _, _) => // going back up from other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(offTarget = offTarget))
              case _ => // TODO: better failure handling of bad json
                throw new RuntimeException("ruh roh")
            }
          case ArrayStart =>
            state match {
              case State(_, _, Nil, TokenFilter.DownArray :: toTarget, onTarget) => // going down towards target
                findDropRanges(chunk, pos + 1, dropPos, state.copy(toTarget = toTarget, onTarget = TokenFilter.DownArray :: onTarget))
              case _ => // going down other path
                findDropRanges(chunk, pos + 1, dropPos, state.copy(offTarget = TokenFilter.DownArray :: state.offTarget))
            }
          case JsonNull | JsonTrue | JsonFalse | _: JsonNumber | _: JsonString =>
            state match {
              case State(_, dropRanges, Nil, Nil, onTarget) => // done dropping
                findDropRanges(chunk, pos + 1, 0, State(dropping = false, dropRanges :+ (dropPos, pos + 1), Nil, Nil, onTarget))
              case State(_, _, TokenFilter.DownField(_) :: offTarget, _, _) =>
                findDropRanges(chunk, pos + 1, dropPos, state.copy(offTarget = offTarget))
              case _ =>
                findDropRanges(chunk, pos + 1, dropPos, state)
            }
        }
      }

    def next(stream: Stream[F, JsonToken], dropping: Boolean, offTarget: List[TokenFilter.Direction], toTarget: List[TokenFilter.Direction], onTarget: List[TokenFilter.Direction]): Pull[F, JsonToken, Unit] =
      stream.pull.unconsChunk.flatMap {
        case Some((jsonTokens, rest)) =>
          val state = findDropRanges(jsonTokens, 0, 0, State(dropping, Vector.empty, offTarget, toTarget, onTarget))
          state.dropRanges match {
            case Vector() =>
              Pull.outputChunk(jsonTokens) >> next(rest, state.dropping, state.offTarget, state.toTarget, state.onTarget)
            case Vector((0, dropEnd)) if dropEnd == jsonTokens.size =>
              next(rest, state.dropping, state.offTarget, state.toTarget, state.onTarget)
            case dropRanges =>
              val (_, lastChunk, result) = dropRanges.foldLeft((0, jsonTokens, Segment.empty[JsonToken])) {
                case ((pos, remaining, acc), (dropStart, dropEnd)) =>
                  (dropEnd, remaining.drop(dropEnd - pos), acc ++ Segment.chunk(remaining.take(dropStart - pos)))
              }
              Pull.output(result ++ Segment.chunk(lastChunk)) >> next(rest, state.dropping, state.offTarget, state.toTarget, state.onTarget)
          }
        case None => Pull.done
      }

    next(_, dropping = false, Nil, target.reverse, Nil).stream
  }

  def insertField[F[_]](fieldName: String): Pipe2[F, JsonToken, Stream[F, JsonToken], JsonToken] = {
    val insertObjectField = ObjectField.fromString(fieldName)

    case class State(insertPositions: Vector[Int], offTarget: List[TokenFilter.Direction], toTarget: List[TokenFilter.Direction], onTarget: List[TokenFilter.Direction])

    @tailrec
    def findInsertPositions(chunk: Chunk[JsonToken], pos: Int, state: State): State =
      if (pos >= chunk.size) {
        state
      } else {
        chunk(pos) match {
          case ObjectStart =>
            state match {
              case State(insertPositions, Nil, TokenFilter.DownObject :: Nil, onTarget) => // found insert pos
                findInsertPositions(chunk, pos + 1, State(insertPositions :+ (pos + 1), Nil, Nil, TokenFilter.DownObject :: onTarget))
              case State(insertPositions, Nil, TokenFilter.DownObject :: toTarget, onTarget) => // down towards target
                findInsertPositions(chunk, pos + 1, State(insertPositions, Nil, toTarget, TokenFilter.DownObject :: onTarget))
              case State(insertPositions, offTarget, toTarget, onTarget) => // down off target
                findInsertPositions(chunk, pos + 1, State(insertPositions, TokenFilter.DownObject :: offTarget, toTarget, onTarget))
            }
          case ObjectEnd =>
            state match {
              case State(insertPositions, Nil, toTarget, TokenFilter.DownObject :: (hd @ TokenFilter.DownField(_)) :: onTarget) => // up from target
                findInsertPositions(chunk, pos + 1, State(insertPositions, Nil, hd :: TokenFilter.DownObject :: toTarget, onTarget))
              case State(insertPositions, Nil, toTarget, TokenFilter.DownObject :: onTarget) => // up from target
                findInsertPositions(chunk, pos + 1, State(insertPositions, Nil, TokenFilter.DownObject :: toTarget, onTarget))
              case State(insertPositions, TokenFilter.DownObject :: TokenFilter.DownField(_) :: offTarget, toTarget, onTarget) => // up off target
                findInsertPositions(chunk, pos + 1, State(insertPositions, offTarget, toTarget, onTarget))
              case State(insertPositions, TokenFilter.DownObject :: offTarget, toTarget, onTarget) => // up off target
                findInsertPositions(chunk, pos + 1, State(insertPositions, offTarget, toTarget, onTarget))
              case _ => // TODO: better failure handling of bad json
                throw new RuntimeException("ruh roh")
            }
          case ArrayStart =>
            state match {
              case State(insertPositions, Nil, TokenFilter.DownArray :: toTarget, onTarget) => // down towards target
                findInsertPositions(chunk, pos + 1, State(insertPositions, Nil, toTarget, TokenFilter.DownArray :: onTarget))
              case State(insertPositions, offTarget, toTarget, onTarget) => // down off target
                findInsertPositions(chunk, pos + 1, State(insertPositions, TokenFilter.DownArray :: offTarget, toTarget, onTarget))
            }
          case ArrayEnd =>
            state match {
              case State(insertPositions, Nil, toTarget, TokenFilter.DownArray :: (hd @ TokenFilter.DownField(_)) :: onTarget) => // up from target
                findInsertPositions(chunk, pos + 1, State(insertPositions, Nil, hd :: TokenFilter.DownArray :: toTarget, onTarget))
              case State(insertPositions, Nil, toTarget, TokenFilter.DownArray :: onTarget) => // up from target
                findInsertPositions(chunk, pos + 1, State(insertPositions, Nil, TokenFilter.DownArray :: toTarget, onTarget))
              case State(insertPositions, TokenFilter.DownArray :: TokenFilter.DownField(_) :: offTarget, toTarget, onTarget) => // up off target
                findInsertPositions(chunk, pos + 1, State(insertPositions, offTarget, toTarget, onTarget))
              case State(insertPositions, TokenFilter.DownArray :: offTarget, toTarget, onTarget) => // up off target
                findInsertPositions(chunk, pos + 1, State(insertPositions, offTarget, toTarget, onTarget))
              case _ => // TODO: better failure handling of bad json
                throw new RuntimeException("ruh roh")
            }
          case objectField: ObjectField =>
            state match {
              case State(insertPositions, Nil, TokenFilter.DownField(`objectField`) :: toTarget, onTarget) => // down towards target
                findInsertPositions(chunk, pos + 1, State(insertPositions, Nil, toTarget, TokenFilter.DownField(`objectField`) :: onTarget))
              case State(insertPositions, offTarget, toTarget, onTarget) => // down off target
                findInsertPositions(chunk, pos + 1, State(insertPositions, TokenFilter.DownField(`objectField`) :: offTarget, toTarget, onTarget))
            }
          case JsonNull | JsonTrue | JsonFalse | _: JsonNumber | _: JsonString =>
            state match {
              case State(_, TokenFilter.DownField(_) :: offTarget, _, _) =>
                findInsertPositions(chunk, pos + 1, state.copy(offTarget = offTarget))
              case _ =>
                findInsertPositions(chunk, pos + 1, state)
            }

        }
      }

    def sendOutput(remaining: Chunk[JsonToken], pos: Int, insertPositions: Vector[Int], insertLeg: Stream.StepLeg[F, Stream[F, JsonToken]]): Pull[F, Stream[F, JsonToken], Option[Stream.StepLeg[F, Stream[F, JsonToken]]]] =
      insertPositions match {
        case Vector() => sendNonEmpty(remaining).as(Some(insertLeg))
        case nextPos +: rest =>
          val (sendNow, sendLater) = remaining.splitAt(nextPos - pos)
          sendNonEmpty(sendNow) >> {
            insertLeg.head.force.uncons1 match {
              case Right((stream, restInsertStream)) =>
                Pull.output1(stream.cons1(ObjectField.fromString(fieldName))) >> sendOutput(sendLater, nextPos, rest, insertLeg.setHead(restInsertStream))
              case Left(()) =>
                insertLeg.stepLeg.flatMap {
                  case Some(nextLeg) =>
                    sendOutput(sendLater, nextPos, insertPositions, nextLeg)
                  case None =>
                    sendNonEmpty(sendLater).as(None)
                }
            }
          }
      }

    def sendNonEmpty(chunk: Chunk[JsonToken]): Pull[F, Stream[F, JsonToken], Unit] =
      if (chunk.nonEmpty)
        Pull.output1(Stream.chunk(chunk).covary[F]).covary[F]
      else
        Pull.pure(()).covary[F]

    def next(tokenLeg: Stream.StepLeg[F, JsonToken], maybeInsertStream: Option[Stream.StepLeg[F, Stream[F, JsonToken]]], offTarget: List[TokenFilter.Direction], toTarget: List[TokenFilter.Direction], onTarget: List[TokenFilter.Direction]): Pull[F, Stream[F, JsonToken], Unit] =
      maybeInsertStream match {
        case Some(insertLeg) =>
          tokenLeg.head.force.unconsChunk match {
            case Right((jsonTokens, remaining)) =>
              val state = findInsertPositions(jsonTokens, 0, State(Vector.empty, offTarget, toTarget, onTarget))
              sendOutput(jsonTokens, 0, state.insertPositions, insertLeg).flatMap { nextInsertStream =>
                tokenLeg.stepLeg.flatMap {
                  case Some(rest) =>
                    next(rest.setHead(remaining ++ rest.head), nextInsertStream, state.offTarget, state.toTarget, state.onTarget)
                  case None =>
                    next(tokenLeg.setHead(remaining), nextInsertStream, state.offTarget, state.toTarget, state.onTarget)
                }
              }
            case Left(()) =>
              Pull.done
          }
        case None => Pull.output1(Stream.segment(tokenLeg.head).covary[F]) >> Pull.output1(tokenLeg.stream)
      }

    (s1, s2) =>
      s1.pull.stepLeg.flatMap {
        case Some(leg1) =>
          s2.pull.stepLeg.flatMap { leg2 =>
            next(leg1, leg2, Nil, target.reverse, Nil)
          }
        case None =>
          Pull.done
      }.stream.flatMap(identity)
  }

}

trait TokenFilterBuilder { parent =>
  def target: List[TokenFilter.Direction]

  def downArray: TokenFilterBuilder = new TokenFilterBuilder {
    override val target: List[TokenFilter.Direction] = TokenFilter.DownArray :: parent.target
  }
  def downObject: ObjectTokenFilterBuilder = new ObjectTokenFilterBuilder {
    override val target: List[TokenFilter.Direction] = TokenFilter.DownObject :: parent.target
  }

}
