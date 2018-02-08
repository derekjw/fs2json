import fs2._

import scala.annotation.{switch, tailrec}
import scala.language.higherKinds

package object fs2json {
  private val numberByte: Set[Byte] = (('0' to '9') ++ Seq('-', '.', '+', 'e', 'E')).map(_.toByte).toSet

  sealed trait ContextState

  case object InObject extends ContextState
  case object InObjectField extends ContextState
  case object InArray extends ContextState

  def tokenParser[F[_]]: Pipe[F, Byte, JsonToken] =
    _.chunks.through(tokenParserC)

  def tokenParserC[F[_]]: Pipe[F, Chunk[Byte], JsonToken] = {

    def dropState(target: ContextState, stack: List[ContextState]): List[ContextState] = stack match {
      case `target` :: rest => rest
      case other => other
    }

    @tailrec
    def parse(pos: Int, byteArray: Array[Byte], output: Vector[JsonToken], stateStack: List[ContextState]): ParserState = {
      if (pos >= byteArray.length) {
        ParserState(output, Chunk.empty, stateStack)
      } else {
        (byteArray(pos): @switch) match {
          case ' ' | '\n' | '\r' | '\t' | ',' | ':' =>
            parse(pos + 1, byteArray, output, stateStack)
          case '{' =>
            parse(pos + 1, byteArray, output :+ ObjectStart, InObject :: dropState(InObjectField, stateStack))
          case '}' if stateStack.headOption.contains(InObject) =>
            parse(pos + 1, byteArray, output :+ ObjectEnd, dropState(InObject, stateStack))
          case '[' =>
            parse(pos + 1, byteArray, output :+ ArrayStart, InArray :: dropState(InObjectField, stateStack))
          case ']' if stateStack.headOption.contains(InArray) =>
            parse(pos + 1, byteArray, output :+ ArrayEnd, dropState(InArray, stateStack))
          case 't' =>
            if (pos + 3 < byteArray.length) { // TODO: verify bytes are correct
              parse(pos + 4, byteArray, output :+ JsonTrue, dropState(InObjectField, stateStack))
            } else {
              ParserState(output, Chunk.bytes(byteArray, pos, byteArray.length - pos), stateStack)
            }
          case 'f' =>
            if (pos + 4 < byteArray.length) { // TODO: verify bytes are correct
              parse(pos + 5, byteArray, output :+ JsonFalse, dropState(InObjectField, stateStack))
            } else {
              ParserState(output, Chunk.bytes(byteArray, pos, byteArray.length - pos), stateStack)
            }
          case 'n' =>
            if (pos + 3 < byteArray.length) { // TODO: verify bytes are correct
              parse(pos + 4, byteArray, output :+ JsonNull, dropState(InObjectField, stateStack))
            } else {
              ParserState(output, Chunk.bytes(byteArray, pos, byteArray.length - pos), stateStack)
            }
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' =>
            val endPos = findNumberEnd(pos, byteArray)
            if (endPos < byteArray.length) {
              val numberString = new String(byteArray.slice(pos, endPos))
              parse(endPos, byteArray, output :+ JsonNumber(numberString), dropState(InObjectField, stateStack))
            } else {
              ParserState(output, Chunk.bytes(byteArray, pos, byteArray.length - pos), stateStack)
            }
          case '"' =>
            if (stateStack.headOption.contains(InObject)) {
              val endPos = findStringEnd(pos + 1, byteArray)
              if (endPos < byteArray.length) {
                val string = new String(byteArray.slice(pos + 1, endPos))
                parse(endPos + 1, byteArray, output :+ ObjectField(string), InObjectField ::stateStack)
              } else {
                ParserState(output, Chunk.bytes(byteArray, pos, byteArray.length - pos), stateStack)
              }
            } else {
              val endPos = findStringEnd(pos + 1, byteArray)
              if (endPos < byteArray.length) {
                val string = new String(byteArray.slice(pos + 1, endPos))
                parse(endPos + 1, byteArray, output :+ JsonString(string), dropState(InObjectField, stateStack))
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
        byteArray(pos) match {
          case 92 => findStringEnd(pos + 2, byteArray)
          case 34 => pos
          case _ => findStringEnd(pos + 1, byteArray)
        }
      } else pos
    }


    case class ParserState(output: Vector[JsonToken], buffer: Chunk[Byte], stateStack: List[ContextState])

    def processSingleChunk(parserState: ParserState, chunk: Chunk[Byte]): ParserState = {
      val allBytes = Array.concat(parserState.buffer.toArray, chunk.toArray)
      parse(0, allBytes, parserState.output, parserState.stateStack)
    }

    def next(buffer: Chunk[Byte], stream: Stream[F, Chunk[Byte]], stateStack: List[ContextState]): Pull[F, JsonToken, Unit] = {
      stream.pull.unconsChunk.flatMap {
        case Some((byteChunks, rest)) =>
          val parserState = byteChunks.foldLeft(ParserState(Vector.empty, buffer, stateStack))(processSingleChunk)
          if (parserState.output.nonEmpty) {
            Pull.output(Segment.seq(parserState.output)) >> next(parserState.buffer, rest, parserState.stateStack)
          } else {
            next(parserState.buffer, rest, parserState.stateStack)
          }
        case None => Pull.done // TODO: fail if buffer is not empty
      }
    }

    next(Chunk.empty, _, Nil).stream
  }
}
