import fs2._

import java.nio.ByteBuffer
import scala.annotation.{switch, tailrec}
import scala.collection.immutable.IntMap
import scala.language.higherKinds

package object fs2json {

  def tokenParser[F[_]]: Pipe[F, Byte, JsonToken] = TokenParser.bytes[F]

  def tokenParserC[F[_]]: Pipe[F, Chunk[Byte], JsonToken] = TokenParser.chunks[F]

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
      stream.pull.uncons.flatMap {
        case Some((tokens, rest)) =>
          val state = tokens.fold(State(Vector.empty, lastToken, level))(processToken).force.run._2
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
