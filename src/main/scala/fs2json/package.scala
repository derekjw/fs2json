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
  private val stringEscape: Byte = '\\'
  private val stringBackspace: Byte = 'b'
  private val stringFormfeed: Byte = 'f'
  private val stringNewline: Byte = 'n'
  private val stringReturn: Byte = 'r'
  private val stringTab: Byte = 't'
  private val stringCheck: Set[Byte] = Set(stringDelimiter, stringEscape)
  private val numberStart: Set[Byte] = (('0' to '9').toSet + '-').map(_.toByte)
  private val numberByte: Set[Byte] = numberStart ++ Seq('.', '+', 'e', 'E').map(_.toByte)

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
          chunk.indexWhere(stringCheck) match {
            case None =>
              stringToken(rest, stateStack, jsonToken, andThen, result :+ chunk)
            case Some(strEnd) =>
              chunk.apply(strEnd) match {
                case `stringDelimiter` =>
                  val strChunk = chunk.take(strEnd)
                  val restChunk = chunk.drop(strEnd + 1)
                  val byteArray = Segment.catenatedChunks(result :+ strChunk).force.toArray
                  Pull.output1(jsonToken(new String(byteArray))) >> andThen(Stream.chunk(restChunk) ++ rest, stateStack)
                case `stringEscape` =>
                  val strChunk = chunk.take(strEnd)
                  val restChunk = chunk.drop(strEnd + 1)
                  rest.cons(Segment.chunk(restChunk)).pull.uncons1.flatMap {
                    case Some((byte, rest2)) => stringUnescape(byte, rest2, stateStack, jsonToken, andThen, result :+ strChunk)
                    case None => Pull.raiseError(TokenParserFailure("ruh roh"))
                  }
              }
          }
        case None =>
          Pull.raiseError(TokenParserFailure("ruh roh"))
      }
    }

    // TODO: Handle unicode escape
    def stringUnescape(byte: Byte, stream: Stream[F, Byte], stateStack: List[State], jsonToken: String => JsonToken, andThen: (Stream[F, Byte], List[State]) => Pull[F, JsonToken, Unit], result: Catenable[Chunk[Byte]]): Pull[F, JsonToken, Unit] = {
      byte match {
        case `stringBackspace` =>
          stringToken(stream, stateStack, jsonToken, andThen, result :+ Chunk[Byte]('\b'))
        case `stringFormfeed` =>
          stringToken(stream, stateStack, jsonToken, andThen, result :+ Chunk[Byte]('\f'))
        case `stringNewline` =>
          stringToken(stream, stateStack, jsonToken, andThen, result :+ Chunk[Byte]('\n'))
        case `stringReturn` =>
          stringToken(stream, stateStack, jsonToken, andThen, result :+ Chunk[Byte]('\r'))
        case `stringTab` =>
          stringToken(stream, stateStack, jsonToken, andThen, result:+ Chunk[Byte]('\t'))
        case other =>
          stringToken(stream, stateStack, jsonToken, andThen, result :+ Chunk[Byte](other))
      }
    }

    def numberToken(stream: Stream[F, Byte], stateStack: List[State], result: Catenable[Chunk[Byte]] = Catenable.empty): Pull[F, JsonToken, Unit] = {
      stream.pull.unconsChunk.flatMap {
        case Some((chunk, rest)) =>
          chunk.indexWhere(byte => !numberByte(byte)) match {
            case None => numberToken(rest, stateStack, result :+ chunk)
            case Some(numEnd) =>
              val (numChunk, restChunk) = chunk.splitAt(numEnd)
              val byteArray = Segment.catenatedChunks(result :+ numChunk).force.toArray
              Pull.output1(JsonNumber(new String(byteArray))) >> next(Stream.chunk(restChunk) ++ rest, stateStack)
          }
        case None =>
          val byteArray = Segment.catenatedChunks(result).force.toArray
          Pull.output1(JsonNumber(new String(byteArray))) >> Pull.done
      }
    }

    def dropState(target: State, stack: List[State]): List[State] = stack match {
      case `target` :: rest => rest
      case other => other
    }

    // TODO: preserve segments when possible
    def next(stream: Stream[F, Byte], stateStack: List[State]): Pull[F, JsonToken, Unit] = {
      stream.pull.uncons1.flatMap {
        case Some((byte, rest)) =>
          byte match {
            case ws if whitespace(ws) =>
              next(rest, stateStack)

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
              numberToken(rest.cons1(number), dropState(InObjectField, stateStack))

            case _ =>
              Pull.raiseError(TokenParserFailure("ruh roh"))
          }
        case None => Pull.done
      }
    }

    next(_, Nil).stream
  }
}
