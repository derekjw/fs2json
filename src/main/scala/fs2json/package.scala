import fs2._

import scala.language.higherKinds

package object fs2json {
  private val whitespace: Set[Byte] = Set(' ', '\n', '\r', '\t', ',', ':').map(_.toByte)

  private val objectStart: Byte = '{'
  private val objectEnd: Byte = '}'
  private val arrayStart: Byte = '['
  private val arrayEnd: Byte = ']'
  private val trueStart: Byte = 't'
  private val trueEnd: Vector[Byte] = Vector('r', 'u', 'e').map(_.toByte)
  private val falseStart: Byte = 'f'
  private val falseEnd: Vector[Byte] = Vector('a', 'l', 's', 'e').map(_.toByte)
  private val nullStart: Byte = 'n'
  private val nullEnd: Vector[Byte] = Vector('u', 'l', 'l').map(_.toByte)
  private val stringDelimiter: Byte = '"'
  private val numberStart: Set[Byte] = (('0' to '9').toSet + '-').map(_.toByte)

  sealed trait State

  case object InObject extends State
  case object InObjectField extends State
  case object InArray extends State

  def tokenParser[F[_]]: Pipe[F, Byte, JsonToken] = {
    def simpleToken(stream: Stream[F, Byte], expected: Vector[Byte], result: JsonToken, stateStack: List[State]): Pull[F, JsonToken, Unit] = {
      stream.pull.unconsN(expected.length).flatMap {
        case Some((rue, rest)) =>
          if (rue.force.toVector == expected) {
            Pull.output1(result) >> next(rest, stateStack)
          } else {
            Pull.raiseError(TokenParserFailure("ruh roh"))
          }
        case None =>
          Pull.raiseError(TokenParserFailure("ruh roh"))
      }
    }

    def stringToken(stream: Stream[F, Byte], stateStack: List[State], jsonToken: String => JsonToken, andThen: (Stream[F, Byte], List[State]) => Pull[F, JsonToken, Unit], result: Catenable[Chunk[Byte]] = Catenable.empty): Pull[F, JsonToken, Unit] = {
      stream.pull.unconsChunk.flatMap {
        case Some((chunk, rest)) =>
          chunk.indexWhere(_ == stringDelimiter) match { // TODO: Handle escape chracter
            case None => stringToken(rest, stateStack, jsonToken, andThen, result :+ chunk)
            case Some(strEnd) =>
              val strChunk = chunk.take(strEnd)
              val restChunk = chunk.drop(strEnd + 1)
              val byteArray = Segment.catenatedChunks(result :+ strChunk).force.toArray
              Pull.output1(jsonToken(new String(byteArray))) >> andThen(Stream.chunk(restChunk) ++ rest, stateStack)
          }
        case None =>
          Pull.raiseError(TokenParserFailure("ruh roh"))
      }
    }

    def dropState(target: State, stack: List[State]): List[State] = stack match {
      case `target` :: rest => rest
      case other => other
    }

    // TODO: preserve segments when possible
    def next(stream: Stream[F, Byte], stateStack: List[State]): Pull[F, JsonToken, Unit] = {
      stream.dropWhile(whitespace).pull.uncons1.flatMap {
        case Some((byte, rest)) =>
          byte match {
            case `objectStart` =>
              Pull.output1(ObjectStart) >> next(rest, InObject :: dropState(InObjectField, stateStack))

            case `objectEnd` if stateStack.headOption.contains(InObject) =>
              Pull.output1(ObjectEnd) >> next(rest, dropState(InObject, stateStack))

            case `arrayStart` =>
              Pull.output1(ArrayStart) >> next(rest, InArray :: dropState(InObjectField, stateStack))

            case `arrayEnd` if stateStack.headOption.contains(InArray) =>
              Pull.output1(ArrayEnd) >> next(rest, dropState(InArray, stateStack))

            case `trueStart` =>
              simpleToken(rest, trueEnd, JsonTrue, dropState(InObjectField, stateStack))

            case `falseStart` =>
              simpleToken(rest, falseEnd, JsonFalse, dropState(InObjectField, stateStack))

            case `nullStart` =>
              simpleToken(rest, nullEnd, JsonNull, dropState(InObjectField, stateStack))

            case `stringDelimiter` if stateStack.headOption.contains(InObject) =>
              stringToken(rest, stateStack, ObjectField, (s, st) => next(s, InObjectField :: st))

            case `stringDelimiter` =>
              stringToken(rest, dropState(InObjectField, stateStack), JsonString, next)

            case number if numberStart(number) =>
              ???
          }
        case None => Pull.done
      }
    }

    next(_, Nil).stream
  }
}
