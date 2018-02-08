package fs2json

import cats.effect.IO
import fs2.{Stream, text}
import utest._
import cats.implicits._

object TokenParserTests extends TestSuite {
  val tests = Tests {
    "should tokenize bad json stream and pretty print results" - {
      val jsonString =
        """
          true
          false     ,
          null
          {}
          ["Hello world!", "World\n\"Hello\"!"]
           {
           "foo": "bar",
           "baz":  {"1" :   -2.1234, "3": "4"}
          }
        """

      val expected =
        """[
          |  true,
          |  false,
          |  null,
          |  {},
          |  [
          |    "Hello world!",
          |    "World\n\"Hello\"!"
          |  ],
          |  {
          |    "foo": "bar",
          |    "baz": {"1":-2.1234,"3":"4"}
          |  }
          |]""".stripMargin

      val result = Stream
        .emit(jsonString)
        .through(text.utf8Encode)
        .through(tokenParser)
        .through(valueStreamToArray)
        .through(prettyPrinter(JsonStyle.SemiPretty(2)))
        .covary[IO]
        .compile
        .foldMonoid
        .unsafeRunSync()

      assert(result == expected)

    }
  }
}
