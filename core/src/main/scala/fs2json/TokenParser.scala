package fs2json

import fs2.{Chunk, Pipe, Pull, Stream}

import scala.annotation.{switch, tailrec}
import scala.language.higherKinds

object TokenParser {
  private val numberByte: Set[Byte] = (('0' to '9') ++ Seq('-', '.', '+', 'e', 'E')).map(_.toByte).toSet

  private sealed trait ContextState

  private case object InObject extends ContextState
  private case object InObjectField extends ContextState
  private case object InArray extends ContextState

  def bytes[F[_]]: Pipe[F, Byte, JsonToken] =
    _.chunks.through(chunks)

  def chunks[F[_]]: Pipe[F, Chunk[Byte], JsonToken] =
    next(Chunk.empty, _, Nil).stream

  private def dropState(target: ContextState, stack: List[ContextState]): List[ContextState] = stack match {
    case `target` :: rest => rest
    case other            => other
  }

  @tailrec
  private def parse(pos: Int, byteArray: Array[Byte], output: Vector[JsonToken], stateStack: List[ContextState], done: Boolean): ParserState =
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
            findStringEnd(pos + 1, byteArray) match {
              case Some(endPos) =>
                parse(endPos, byteArray, output :+ ObjectField(Chunk.Bytes(byteArray, pos, endPos - pos)), InObjectField :: stateStack, done)
              case None =>
                ParserState(output, Chunk.bytes(byteArray, pos, byteArray.length - pos), stateStack)
            }
          } else {
            findStringEnd(pos + 1, byteArray) match {
              case Some(endPos) =>
                parse(endPos, byteArray, output :+ JsonString(Chunk.Bytes(byteArray, pos, endPos - pos)), dropState(InObjectField, stateStack), done)
              case None =>
                ParserState(output, Chunk.bytes(byteArray, pos, byteArray.length - pos), stateStack)
            }
          }
      }
    }

  @tailrec
  private def findNumberEnd(pos: Int, byteArray: Array[Byte]): Int =
    if (pos < byteArray.length && numberByte(byteArray(pos))) findNumberEnd(pos + 1, byteArray) else pos

  @tailrec
  private def findStringEnd(pos: Int, byteArray: Array[Byte]): Option[Int] =
    if (pos < byteArray.length) {
      (byteArray(pos): @switch) match {
        case '\\' => findStringEnd(pos + 2, byteArray)
        case '"'  => Some(pos + 1)
        case _    => findStringEnd(pos + 1, byteArray)
      }
    } else None

  private case class ParserState(output: Vector[JsonToken], buffer: Chunk[Byte], stateStack: List[ContextState])

  private def processSingleChunk(parserState: ParserState, chunk: Chunk[Byte]): ParserState = {
    val allBytes = Array.concat(parserState.buffer.toArray, chunk.toArray)
    parse(0, allBytes, parserState.output, parserState.stateStack, done = false)
  }

  private def processRemaining(parserState: ParserState): ParserState =
    parse(0, parserState.buffer.toArray, parserState.output, parserState.stateStack, done = true)

  private def next[F[_]](buffer: Chunk[Byte], stream: Stream[F, Chunk[Byte]], stateStack: List[ContextState]): Pull[F, JsonToken, Unit] =
    stream.pull.uncons.flatMap {
      case Some((byteChunks, rest)) =>
        val parserState = byteChunks.foldLeft(ParserState(Vector.empty, buffer, stateStack))(processSingleChunk)
        if (parserState.output.nonEmpty) {
          Pull.output(Chunk.seq(parserState.output)) >> next(parserState.buffer, rest, parserState.stateStack)
        } else {
          next(parserState.buffer, rest, parserState.stateStack)
        }
      case None =>
        val parserState = processRemaining(ParserState(Vector.empty, buffer, stateStack))
        // TODO: fail if buffer is not empty
        if (parserState.output.nonEmpty) {
          Pull.output(Chunk.seq(parserState.output)) >> Pull.done
        } else {
          Pull.done
        }
    }

}
