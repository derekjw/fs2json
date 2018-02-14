import fs2._

import java.nio.ByteBuffer
import scala.annotation.{switch, tailrec}
import scala.collection.immutable.IntMap
import scala.language.higherKinds

package object fs2json {
  private val numberByte: Set[Byte] = (('0' to '9') ++ Seq('-', '.', '+', 'e', 'E')).map(_.toByte).toSet

  private sealed trait ContextState

  private case object InObject extends ContextState
  private case object InObjectField extends ContextState
  private case object InArray extends ContextState

  def tokenParser[F[_]]: Pipe[F, Byte, JsonToken] =
    _.chunks.through(tokenParserC)

  def tokenParserC[F[_]]: Pipe[F, Chunk[Byte], JsonToken] = {

    def dropState(target: ContextState, stack: List[ContextState]): List[ContextState] = stack match {
      case `target` :: rest => rest
      case other => other
    }

    @tailrec
    def parse(pos: Int, byteArray: Array[Byte], output: Vector[JsonToken], stateStack: List[ContextState], done: Boolean): ParserState = {
      if (pos >= byteArray.length) {
        ParserState(output, Chunk.empty, stateStack)
      } else {
        (byteArray(pos): @switch) match {
          case ' ' | '\n' | '\r' | '\t' | ',' | ':' =>
            parse(pos + 1, byteArray, output, stateStack, done)
          case '{' =>
            parse(pos + 1, byteArray, output :+ ObjectStart, InObject :: dropState(InObjectField, stateStack), done)
          case '}' if stateStack.headOption.contains(InObject) =>
            parse(pos + 1, byteArray, output :+ ObjectEnd, dropState(InObject, stateStack), done)
          case '[' =>
            parse(pos + 1, byteArray, output :+ ArrayStart, InArray :: dropState(InObjectField, stateStack), done)
          case ']' if stateStack.headOption.contains(InArray) =>
            parse(pos + 1, byteArray, output :+ ArrayEnd, dropState(InArray, stateStack), done)
          case 't' =>
            if (pos + 3 < byteArray.length) { // TODO: verify bytes are correct
              parse(pos + 4, byteArray, output :+ JsonTrue, dropState(InObjectField, stateStack), done)
            } else {
              ParserState(output, Chunk.bytes(byteArray, pos, byteArray.length - pos), stateStack)
            }
          case 'f' =>
            if (pos + 4 < byteArray.length) { // TODO: verify bytes are correct
              parse(pos + 5, byteArray, output :+ JsonFalse, dropState(InObjectField, stateStack), done)
            } else {
              ParserState(output, Chunk.bytes(byteArray, pos, byteArray.length - pos), stateStack)
            }
          case 'n' =>
            if (pos + 3 < byteArray.length) { // TODO: verify bytes are correct
              parse(pos + 4, byteArray, output :+ JsonNull, dropState(InObjectField, stateStack), done)
            } else {
              ParserState(output, Chunk.bytes(byteArray, pos, byteArray.length - pos), stateStack)
            }
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' =>
            val endPos = findNumberEnd(pos, byteArray)
            if (endPos < byteArray.length || done) {
              parse(endPos, byteArray, output :+ JsonNumber(Chunk.Bytes(byteArray, pos, endPos - pos)), dropState(InObjectField, stateStack), done)
            } else {
              ParserState(output, Chunk.bytes(byteArray, pos, byteArray.length - pos), stateStack)
            }
          case '"' =>
            if (stateStack.headOption.contains(InObject)) {
              val endPos = findStringEnd(pos + 1, byteArray)
              if (endPos < byteArray.length) {
                parse(endPos, byteArray, output :+ ObjectField(Chunk.Bytes(byteArray, pos, endPos - pos)), InObjectField :: stateStack, done)
              } else {
                ParserState(output, Chunk.bytes(byteArray, pos, byteArray.length - pos), stateStack)
              }
            } else {
              val endPos = findStringEnd(pos + 1, byteArray)
              if (endPos <= byteArray.length) {
                parse(endPos, byteArray, output :+ JsonString(Chunk.Bytes(byteArray, pos, endPos - pos)), dropState(InObjectField, stateStack), done)
              } else {
                ParserState(output, Chunk.bytes(byteArray, pos, byteArray.length - pos), stateStack)
              }
            }
        }
      }
    }

    @tailrec
    def findNumberEnd(pos: Int, byteArray: Array[Byte]): Int =
      if (pos < byteArray.length && numberByte(byteArray(pos))) findNumberEnd(pos + 1, byteArray) else pos

    @tailrec
    def findStringEnd(pos: Int, byteArray: Array[Byte]): Int = {
      if (pos < byteArray.length) {
        (byteArray(pos): @switch) match {
          case '\\' => findStringEnd(pos + 2, byteArray)
          case '"' => pos + 1
          case _ => findStringEnd(pos + 1, byteArray)
        }
      } else pos
    }


    case class ParserState(output: Vector[JsonToken], buffer: Chunk[Byte], stateStack: List[ContextState])

    def processSingleChunk(parserState: ParserState, chunk: Chunk[Byte]): ParserState = {
      val allBytes = Array.concat(parserState.buffer.toArray, chunk.toArray)
      parse(0, allBytes, parserState.output, parserState.stateStack, done = false)
    }

    def processRemaining(parserState: ParserState): ParserState =
      parse(0, parserState.buffer.toArray, parserState.output, parserState.stateStack, done = true)

    def next(buffer: Chunk[Byte], stream: Stream[F, Chunk[Byte]], stateStack: List[ContextState]): Pull[F, JsonToken, Unit] = {
      stream.pull.unconsChunk.flatMap {
        case Some((byteChunks, rest)) =>
          val parserState = byteChunks.foldLeft(ParserState(Vector.empty, buffer, stateStack))(processSingleChunk)
          if (parserState.output.nonEmpty) {
            Pull.output(Segment.seq(parserState.output)) >> next(parserState.buffer, rest, parserState.stateStack)
          } else {
            next(parserState.buffer, rest, parserState.stateStack)
          }
        case None =>
          val parserState = processRemaining(ParserState(Vector.empty, buffer, stateStack))
          // TODO: fail if buffer is not empty
          if (parserState.output.nonEmpty) {
            Pull.output(Segment.seq(parserState.output)) >> Pull.done
          } else {
            Pull.done
          }
      }
    }

    next(Chunk.empty, _, Nil).stream
  }

  def valueStreamToArray[F[_]]: fs2.Pipe[F, JsonToken, JsonToken] = Stream.emit(ArrayStart) ++ _ ++ Stream.emit(ArrayEnd)

  def prettyPrinter[F[_]](jsonStyle: JsonStyle = JsonStyle.NoSpaces): fs2.Pipe[F, JsonToken, Byte] = { stream =>
    case class State(output: Vector[Chunk.Bytes], lastToken: Option[JsonToken] = None, level: Int = 0)

    val space = Chunk.Bytes(Array[Byte](' '))
    val colon = Chunk.Bytes(Array[Byte](':'))
    val comma = Chunk.Bytes(Array[Byte](','))
    var indents = IntMap.empty[Chunk.Bytes]

    def getIndent(level: Int) = indents.get(level) match {
      case Some(s) => s
      case None =>
        val arr = Array.fill[Byte](1 + (level * 2))(' ')
        arr(0) = '\n'
        val chunk = Chunk.Bytes(arr)
        indents += (level -> chunk)
        chunk
    }

    def indent(output: Vector[Chunk.Bytes], level: Int, endToken: Boolean): Vector[Chunk.Bytes] = jsonStyle match {
      case JsonStyle.Pretty => output :+ getIndent(if (endToken) level - 1 else level)
      case JsonStyle.SemiPretty(levelLimit) if level <= levelLimit => output :+ getIndent(if (endToken) level - 1 else level)
      case _ if level == 0 => output :+ getIndent(0) // value stream, always new line
      case _ => output
    }

    def fieldSpace(output: Vector[Chunk.Bytes], level: Int): Vector[Chunk.Bytes] = jsonStyle match {
      case JsonStyle.Pretty => output :+ space
      case JsonStyle.SemiPretty(levelLimit) if level <= levelLimit => output :+ space
      case _ => output
    }

    def formatOutput(token: JsonToken, state: State) = {
      (token match {
        case ObjectEnd | ArrayEnd =>
          state.lastToken match {
            case Some(ObjectStart | ArrayStart) => state.output
            case _ => indent(state.output, state.level, endToken = true)
          }
        case _ => state.lastToken match {
          case Some(ObjectStart | ArrayStart) => indent(state.output, state.level, endToken = false)
          case Some(_: ObjectField) => fieldSpace(state.output :+ colon, state.level)
          case None => state.output
          case _ if state.level == 0 => indent(state.output, 0, endToken = false)
          case _ => indent(state.output :+ comma, state.level, endToken = false)
        }
      }) :+ token.value
    }

    def processToken(state: State, token: JsonToken): State = {
      val nextLevel = token match {
        case ObjectStart | ArrayStart => state.level + 1
        case ObjectEnd | ArrayEnd => state.level - 1
        case _ => state.level
      }
      State(formatOutput(token, state), Some(token), nextLevel)
    }

    def next(stream: fs2.Stream[F, JsonToken], lastToken: Option[JsonToken] = None, level: Int = 0): fs2.Pull[F, Byte, Unit] = {
      stream.pull.unconsChunk.flatMap {
        case Some((tokens, rest)) =>
          val state = tokens.foldLeft(State(Vector.empty, lastToken, level))(processToken)
          val outputSize = state.output.foldLeft(0)(_ + _.size)
          val outputBuffer = ByteBuffer.allocate(outputSize)
          state.output.foreach { chunk =>
            outputBuffer.put(chunk.toByteBuffer)
          }
          outputBuffer.flip()
          val compacted = Chunk.byteBuffer(outputBuffer)
          Pull.outputChunk(compacted) >> next(rest, state.lastToken, state.level)
        case None => fs2.Pull.done
      }
    }

    next(stream).stream
  }

}
