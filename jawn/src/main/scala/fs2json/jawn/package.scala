package fs2json

import java.nio.ByteBuffer

import fs2.{Pipe, Pull, Segment, Stream}
import _root_.jawn.{ByteBufferParser, FContext, Facade}

import scala.annotation.switch
import scala.language.higherKinds

package object jawn {
  def valueStream[F[_], J](implicit facade: Facade[J]): Pipe[F, JsonToken, J] = {
    case class State(output: Vector[J], stack: List[FContext[J]])

    def processToken(state: State, jsonToken: JsonToken): State = jsonToken match {
      case ObjectStart =>
        state.copy(stack = facade.objectContext() :: state.stack)
      case ObjectEnd =>
        state.stack match {
          case ctx :: parent :: rest =>
            val obj = ctx.finish
            parent.add(obj)
            state.copy(stack = parent :: rest)
          case ctx :: Nil =>
            val obj = ctx.finish
            state.copy(output = state.output :+ obj, stack = Nil)
          case Nil =>
            throw JawnParserFailure("Unexpected end of object")
        }
      case ArrayStart =>
        state.copy(stack = facade.arrayContext() :: state.stack)
      case ArrayEnd =>
        state.stack match {
          case ctx :: parent :: rest =>
            val array = ctx.finish
            parent.add(array)
            state.copy(stack = parent :: rest)
          case ctx :: Nil =>
            val array = ctx.finish
            state.copy(output = state.output :+ array, stack = Nil)
          case Nil =>
            throw JawnParserFailure("Unexpected end of array")
        }
      case objectField: ObjectField =>
        val str = parseString(objectField.value.toByteBuffer)
        state.stack match {
          case ctx :: _ =>
            ctx.add(str)
            state
          case Nil =>
            state.copy(output = state.output :+ facade.jstring(str))
        }
      case jsonString: JsonString =>
        val str = parseString(jsonString.value.toByteBuffer)
        state.stack match {
          case ctx :: _ =>
            ctx.add(str)
            state
          case Nil =>
            state.copy(output = state.output :+ facade.jstring(str))
        }
      case token =>
        // TODO: do our own parsing
        val jValue = new ByteBufferParser[J](token.value.toByteBuffer).parse
        state.stack match {
          case ctx :: _ =>
            ctx.add(jValue)
            state
          case Nil =>
            state.copy(output = state.output :+ jValue)
        }
    }

    def next(stream: Stream[F, JsonToken], stack: List[FContext[J]]): Pull[F, J, Unit] =
      stream.pull.uncons.flatMap {
        case Some((jsonTokens, rest)) =>
          val state = jsonTokens.fold(State(Vector.empty, stack))(processToken).force.run._2
          if (state.output.nonEmpty) {
            Pull.output(Segment.seq(state.output)) >> next(rest, state.stack)
          } else {
            Pull.suspend(next(rest, state.stack))
          }
        case None => Pull.done // TODO: finish stack
      }

    next(_, Nil).stream
  }

  // taken from Jawn ByteBufferParser
  private def parseString(src: ByteBuffer): String = {
    val start = src.position()
    def byte(i: Int): Byte = src.get(i + start)
    def char(i: Int): Char = src.get(i + start).toChar

    def at(i: Int, k: Int): String = {
      val len = k - i
      val arr = new Array[Byte](len)
      src.position(i + start)
      src.get(arr, 0, len)
      src.position(start)
      new String(arr, "UTF-8")
    }

    /**
      * See if the string has any escape sequences. If not, return the end of the
      * string. If so, bail out and return -1.
      *
      * This method expects the data to be in UTF-8 and accesses it as bytes. Thus
      * we can just ignore any bytes with the highest bit set.
      */
    def parseStringSimple(i: Int): Int = {
      var j = i
      var c: Int = byte(j) & 0xff
      while (c != 34) {
        if (c < 32) throw JawnParserFailure(s"control char ($c) in string")
        if (c == 92) return -1
        j += 1
        c = byte(j) & 0xff
      }
      j + 1
    }

    /**
      * Parse the string according to JSON rules, and add to the given context.
      *
      * This method expects the data to be in UTF-8 and accesses it as bytes.
      */
    def parseString(i: Int): String = {
      val k = parseStringSimple(i + 1)
      if (k != -1) {
        return at(i + 1, k - 1)
      }

      // TODO: we might be able to do better by identifying where
      // escapes occur, and then translating the intermediate strings in
      // one go.

      var j = i + 1
      val sb = new CharBuilder

      var c: Int = byte(j) & 0xff
      while (c != 34) { // "
        if (c == 92) { // \
          (byte(j + 1): @switch) match {
            case 98  => { sb.append('\b'); j += 2 }
            case 102 => { sb.append('\f'); j += 2 }
            case 110 => { sb.append('\n'); j += 2 }
            case 114 => { sb.append('\r'); j += 2 }
            case 116 => { sb.append('\t'); j += 2 }

            case 34 => { sb.append('"'); j += 2 }
            case 47 => { sb.append('/'); j += 2 }
            case 92 => { sb.append('\\'); j += 2 }

            // if there's a problem then descape will explode
            case 117 => { sb.append(descape(at(j + 2, j + 6))); j += 6 }

            case c => throw JawnParserFailure(s"invalid escape sequence (\\${c.toChar})")
          }
        } else if (c < 32) {
          throw JawnParserFailure(s"control char ($c) in string")
        } else if (c < 128) {
          // 1-byte UTF-8 sequence
          sb.append(c.toChar)
          j += 1
        } else if ((c & 224) == 192) {
          // 2-byte UTF-8 sequence
          sb.extend(at(j, j + 2))
          j += 2
        } else if ((c & 240) == 224) {
          // 3-byte UTF-8 sequence
          sb.extend(at(j, j + 3))
          j += 3
        } else if ((c & 248) == 240) {
          // 4-byte UTF-8 sequence
          sb.extend(at(j, j + 4))
          j += 4
        } else {
          throw JawnParserFailure("invalid UTF-8 encoding")
        }
        c = byte(j) & 0xff
      }

      sb.makeString
    }

    parseString(0)
  }

  private val HexChars: Array[Int] = {
    val arr = new Array[Int](128)
    var i = 0
    while (i < 10) { arr(i + '0') = i; i += 1 }
    i = 0
    while (i < 16) { arr(i + 'a') = 10 + i; arr(i + 'A') = 10 + i; i += 1 }
    arr
  }

  private def descape(s: CharSequence): Char = {
    val hc = HexChars
    var i = 0
    var x = 0
    while (i < 4) {
      x = (x << 4) | hc(s.charAt(i).toInt)
      i += 1
    }
    x.toChar
  }

  private final class CharBuilder {
    @inline final def INITIALSIZE = 32

    private var cs = new Array[Char](INITIALSIZE)
    private var capacity = INITIALSIZE
    private var len = 0

    def reset(): CharBuilder = {
      len = 0
      this
    }

    def makeString: String = new String(cs, 0, len)

    def resizeIfNecessary(goal: Int): Unit = {
      if (goal <= capacity) return ()
      var cap = capacity
      while (goal > cap && cap > 0) cap *= 2
      if (cap > capacity) {
        val ncs = new Array[Char](cap)
        System.arraycopy(cs, 0, ncs, 0, capacity)
        cs = ncs
        capacity = cap
      } else if (cap < capacity) {
        sys.error("maximum string size exceeded")
      }
    }

    def extend(s: CharSequence): Unit = {
      val tlen = len + s.length
      resizeIfNecessary(tlen)
      var i = 0
      var j = len
      len = tlen
      while (i < s.length) {
        cs(j) = s.charAt(i)
        i += 1
        j += 1
      }
    }

    def append(c: Char): Unit = {
      val tlen = len + 1
      resizeIfNecessary(tlen)
      cs(len) = c
      len = tlen
    }
  }

}
