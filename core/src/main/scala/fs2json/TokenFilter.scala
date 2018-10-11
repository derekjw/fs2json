package fs2json

import fs2.{Chunk, _}

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

    def next(stream: Stream[F, JsonToken],
             dropping: Boolean,
             offTarget: List[TokenFilter.Direction],
             toTarget: List[TokenFilter.Direction],
             onTarget: List[TokenFilter.Direction]): Pull[F, JsonToken, Unit] =
      stream.pull.uncons.flatMap {
        case Some((jsonTokens, rest)) =>
          val state = findDropRanges(jsonTokens, 0, 0, State(dropping, Vector.empty, offTarget, toTarget, onTarget))
          state.dropRanges match {
            case Vector() =>
              Pull.output(jsonTokens) >> next(rest, state.dropping, state.offTarget, state.toTarget, state.onTarget)
            case Vector((0, dropEnd)) if dropEnd == jsonTokens.size =>
              next(rest, state.dropping, state.offTarget, state.toTarget, state.onTarget)
            case dropRanges =>
              val (_, lastChunk, result) = dropRanges.foldLeft((0, jsonTokens, Chunk.empty[JsonToken])) {
                case ((pos, remaining, acc), (dropStart, dropEnd)) =>
                  (dropEnd, remaining.drop(dropEnd - pos), Chunk.concat(List(acc, remaining.take(dropStart - pos)))) // TODO: effecient concat
              }
              Pull.output(Chunk.concat(List(result, lastChunk))) >> next(rest, state.dropping, state.offTarget, state.toTarget, state.onTarget)
          }
        case None => Pull.done
      }

    next(_, dropping = false, Nil, target.reverse, Nil).stream
  }

  def insertField[F[_]](fieldName: String): Pipe2[F, JsonToken, Stream[F, JsonToken], JsonToken] = {
    val insertObjectField = ObjectField.fromString(fieldName)
    val unit: Pull[F, JsonToken, Unit] = Pull.pure(())

    case class State(offTarget: List[TokenFilter.Direction], toTarget: List[TokenFilter.Direction], onTarget: List[TokenFilter.Direction])

    @tailrec
    def findInsertPosition(chunk: Chunk[JsonToken], pos: Int, state: State): (Option[Int], State) =
      if (pos >= chunk.size) {
        (None, state)
      } else {
        chunk(pos) match {
          case ObjectStart =>
            state match {
              case State(Nil, TokenFilter.DownObject :: Nil, onTarget) => // found insert pos
                Some(pos + 1) -> State(Nil, Nil, TokenFilter.DownObject :: onTarget)
              case State(Nil, TokenFilter.DownObject :: toTarget, onTarget) => // down towards target
                findInsertPosition(chunk, pos + 1, State(Nil, toTarget, TokenFilter.DownObject :: onTarget))
              case State(offTarget, toTarget, onTarget) => // down off target
                findInsertPosition(chunk, pos + 1, State(TokenFilter.DownObject :: offTarget, toTarget, onTarget))
            }
          case ObjectEnd =>
            state match {
              case State(Nil, toTarget, TokenFilter.DownObject :: (hd @ TokenFilter.DownField(_)) :: onTarget) => // up from target
                findInsertPosition(chunk, pos + 1, State(Nil, hd :: TokenFilter.DownObject :: toTarget, onTarget))
              case State(Nil, toTarget, TokenFilter.DownObject :: onTarget) => // up from target
                findInsertPosition(chunk, pos + 1, State(Nil, TokenFilter.DownObject :: toTarget, onTarget))
              case State(TokenFilter.DownObject :: TokenFilter.DownField(_) :: offTarget, toTarget, onTarget) => // up off target
                findInsertPosition(chunk, pos + 1, State(offTarget, toTarget, onTarget))
              case State(TokenFilter.DownObject :: offTarget, toTarget, onTarget) => // up off target
                findInsertPosition(chunk, pos + 1, State(offTarget, toTarget, onTarget))
              case other => // TODO: better failure handling of bad json
                throw new RuntimeException("ruh roh")
            }
          case ArrayStart =>
            state match {
              case State(Nil, TokenFilter.DownArray :: toTarget, onTarget) => // down towards target
                findInsertPosition(chunk, pos + 1, State(Nil, toTarget, TokenFilter.DownArray :: onTarget))
              case State(offTarget, toTarget, onTarget) => // down off target
                findInsertPosition(chunk, pos + 1, State(TokenFilter.DownArray :: offTarget, toTarget, onTarget))
            }
          case ArrayEnd =>
            state match {
              case State(Nil, toTarget, TokenFilter.DownArray :: (hd @ TokenFilter.DownField(_)) :: onTarget) => // up from target
                findInsertPosition(chunk, pos + 1, State(Nil, hd :: TokenFilter.DownArray :: toTarget, onTarget))
              case State(Nil, toTarget, TokenFilter.DownArray :: onTarget) => // up from target
                findInsertPosition(chunk, pos + 1, State(Nil, TokenFilter.DownArray :: toTarget, onTarget))
              case State(TokenFilter.DownArray :: TokenFilter.DownField(_) :: offTarget, toTarget, onTarget) => // up off target
                findInsertPosition(chunk, pos + 1, State(offTarget, toTarget, onTarget))
              case State(TokenFilter.DownArray :: offTarget, toTarget, onTarget) => // up off target
                findInsertPosition(chunk, pos + 1, State(offTarget, toTarget, onTarget))
              case _ => // TODO: better failure handling of bad json
                throw new RuntimeException("ruh roh")
            }
          case objectField: ObjectField =>
            state match {
              case State(Nil, TokenFilter.DownField(`objectField`) :: toTarget, onTarget) => // down towards target
                findInsertPosition(chunk, pos + 1, State(Nil, toTarget, TokenFilter.DownField(`objectField`) :: onTarget))
              case State(offTarget, toTarget, onTarget) => // down off target
                findInsertPosition(chunk, pos + 1, State(TokenFilter.DownField(`objectField`) :: offTarget, toTarget, onTarget))
            }
          case JsonNull | JsonTrue | JsonFalse | _: JsonNumber | _: JsonString =>
            state match {
              case State(TokenFilter.DownField(_) :: offTarget, _, _) =>
                findInsertPosition(chunk, pos + 1, state.copy(offTarget = offTarget))
              case _ =>
                findInsertPosition(chunk, pos + 1, state)
            }

        }
      }

    def sendOutput(insertLeg: Stream.StepLeg[F, Stream[F, JsonToken]]): Pull[F, JsonToken, Option[Stream.StepLeg[F, Stream[F, JsonToken]]]] =
      insertLeg.head match {
        case chunk if chunk.nonEmpty =>
          val stream = chunk(0)
          val restInsertStream = chunk.drop(1)
          Pull.output1(insertObjectField) >>
            stream.pull.stepLeg
              .flatMap(_.fold(unit)(sendInsertLeg))
              .as(Some(insertLeg.setHead(restInsertStream)))
        case _ =>
          insertLeg.stepLeg.flatMap {
            case Some(nextLeg) =>
              sendOutput(nextLeg)
            case None =>
              Pull.pure(None)
          }
      }

    def sendInsertLeg(leg: Stream.StepLeg[F, JsonToken]): Pull[F, JsonToken, Unit] =
      Pull.output(leg.head) >> leg.stepLeg.flatMap(_.fold(unit)(sendInsertLeg))

    def sendNonEmpty(chunk: Chunk[JsonToken]): Pull[F, JsonToken, Unit] =
      if (chunk.nonEmpty)
        Pull.output(chunk)
      else
        Pull.pure(())

    def next(tokenLeg: Stream.StepLeg[F, JsonToken], maybeInsertsLeg: Option[Stream.StepLeg[F, Stream[F, JsonToken]]], state: State): Pull[F, JsonToken, Unit] =
      maybeInsertsLeg match {
        case Some(insertsLeg) =>
          tokenLeg.head match {
            case jsonTokens if jsonTokens.nonEmpty =>
              val (pos, nextState) = findInsertPosition(jsonTokens, 0, state)
              pos match {
                case Some(p) =>
                  val (sendNow, sendLater) = jsonTokens.splitAt(p)
                  sendNonEmpty(sendNow) >>
                    sendOutput(insertsLeg).flatMap { restInsertStream =>
                      next(tokenLeg.setHead(sendLater), restInsertStream, nextState)
                    }

                case None =>
                  sendNonEmpty(jsonTokens) >>
                    tokenLeg.stepLeg.flatMap {
                      case Some(rest) =>
                        next(rest, maybeInsertsLeg, nextState)
                      case None =>
                        Pull.done
                    }
              }
            case _ =>
              tokenLeg.stepLeg.flatMap {
                case Some(rest) =>
                  next(rest, maybeInsertsLeg, state)
                case None =>
                  Pull.done
              }
          }
        case None => Pull.output(tokenLeg.head) >> tokenLeg.stream.pull.echo
      }

    (s1, s2) =>
      s1.pull.stepLeg.flatMap {
        case Some(leg1) =>
          s2.pull.stepLeg.flatMap { leg2 =>
            next(leg1, leg2, State(Nil, target.reverse, Nil))
          }
        case None =>
          Pull.done
      }.stream
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
