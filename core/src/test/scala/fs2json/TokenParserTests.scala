package fs2json

import cats.effect.IO
import fs2.{Pure, Stream, text}
import utest._
import cats.implicits._
import io.circe.Json
import io.circe.parser.parse
import io.circe.testing.instances._
import org.scalacheck.Prop.forAll

object TokenParserTests extends TestSuite with UTestScalaCheck {
  val tests = Tests {
    "round trip" - {
      def chunked(json: Json): Stream[Pure, Byte] = Stream.emit(json.noSpaces).through(text.utf8Encode)
      def unchunked(json: Json): Stream[Pure, Byte] = chunked(json).unchunk

      def roundTrip(json: Json, style: JsonStyle = JsonStyle.NoSpaces, unchunk: Boolean = false): Json =
        (if (unchunk) unchunked(json) else chunked(json))
          .covary[IO]
          .through(tokenParser)
          .through(prettyPrinter(style))
          .through(text.utf8Decode)
          .compile
          .foldMonoid
          .map(parse)
          .map(_.valueOr(throw _))
          .unsafeRunSync

      "json" - {
        "chunked" - {
          "pretty printed" -
            forAll { (json: Json) =>
              roundTrip(json, JsonStyle.Pretty) == json
            }.checkUTest()

          "noSpaces printed" -
            forAll { (json: Json) =>
              roundTrip(json, JsonStyle.NoSpaces) == json
            }.checkUTest()
        }

        "unchunked" - {
          "pretty printed" -
            forAll { (json: Json) =>
              roundTrip(json, JsonStyle.Pretty, unchunk = true) == json
            }.checkUTest()

          "noSpaces printed" -
            forAll { (json: Json) =>
              roundTrip(json, JsonStyle.NoSpaces, unchunk = true) == json
            }.checkUTest()
        }
      }

    }

    "tokenize and pretty print" - {
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

      "chunked into array" - {
        val result = Stream
          .emit(jsonString)
          .through(text.utf8Encode)
          .through(tokenParser)
          .through(valueStreamToArray)
          .through(prettyPrinter(JsonStyle.SemiPretty(2)))
          .through(text.utf8Decode)
          .covary[IO]
          .compile
          .foldMonoid
          .unsafeRunSync()

        assert(result == expected)
      }

      "unchunked into array" - {
        val result = Stream
          .emit(jsonString)
          .through(text.utf8Encode)
          .unchunk
          .through(tokenParser)
          .through(valueStreamToArray)
          .through(prettyPrinter(JsonStyle.SemiPretty(2)))
          .through(text.utf8Decode)
          .covary[IO]
          .compile
          .foldMonoid
          .unsafeRunSync()

        assert(result == expected)
      }

      "value stream" - {
        val result = Stream
          .emit(jsonString)
          .through(text.utf8Encode)
          .through(tokenParser)
          .through(prettyPrinter(JsonStyle.SemiPretty(1)))
          .through(text.utf8Decode)
          .covary[IO]
          .compile
          .foldMonoid
          .unsafeRunSync()

        assert(result == """true
                           |false
                           |null
                           |{}
                           |[
                           |  "Hello world!",
                           |  "World\n\"Hello\"!"
                           |]
                           |{
                           |  "foo": "bar",
                           |  "baz": {"1":-2.1234,"3":"4"}
                           |}""".stripMargin)
      }

      "value stream noSpaces" - {
        val result = Stream
          .emit(jsonString)
          .through(text.utf8Encode)
          .through(tokenParser)
          .through(prettyPrinter(JsonStyle.NoSpaces))
          .through(text.utf8Decode)
          .covary[IO]
          .compile
          .foldMonoid
          .unsafeRunSync()

        assert(result == """true
                           |false
                           |null
                           |{}
                           |["Hello world!","World\n\"Hello\"!"]
                           |{"foo":"bar","baz":{"1":-2.1234,"3":"4"}}""".stripMargin)
      }

    }

    "TokenFilter" - {
      val jsonString =
        """[
          |{"foo" : {
          |  "a": { "1": 1, "2" : true, "3" : 3 },
          |  "b": { "1": 1, "2" : true, "3" : 3 },
          |},
          |"bar" : {
          |  "a": { "1": 1, "2" : true, "3" : 3 },
          |  "b": { "1": 1, "2" : true, "3" : 3 },
          |}
          |},
          |{"foo" : {
          |  "a": { "1": 1, "2" : true, "3" : 3 },
          |  "b": { "1": 1, "2" : true, "3" : 3 },
          |},
          |"bar" : {
          |  "a": { "1": 1, "2" : true, "3" : 3 },
          |  "b": { "1": 1, "2" : true, "3" : 3 },
          |}
          |}
          |]
        """.stripMargin

      "dropping fields" - {
        "high level" - {
          val result = Stream
            .emit(jsonString)
            .through(text.utf8Encode)
            .through(tokenParser)
            .through(TokenFilter.downArray.downObject.downField("foo").downObject.removeField("a"))
            .through(prettyPrinter(JsonStyle.SemiPretty(3)))
            .through(text.utf8Decode)
            .covary[IO]
            .compile
            .foldMonoid
            .unsafeRunSync()

          assert(result == """[
                             |  {
                             |    "foo": {
                             |      "b": {"1":1,"2":true,"3":3}
                             |    },
                             |    "bar": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    }
                             |  },
                             |  {
                             |    "foo": {
                             |      "b": {"1":1,"2":true,"3":3}
                             |    },
                             |    "bar": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    }
                             |  }
                             |]""".stripMargin)

        }

        "single value" - {
          val result = Stream
            .emit(jsonString)
            .through(text.utf8Encode)
            .through(tokenParser)
            .through(TokenFilter.downArray.downObject.downField("foo").downObject.downField("a").downObject.removeField("2"))
            .through(prettyPrinter(JsonStyle.SemiPretty(3)))
            .through(text.utf8Decode)
            .covary[IO]
            .compile
            .foldMonoid
            .unsafeRunSync()

          assert(result == """[
                             |  {
                             |    "foo": {
                             |      "a": {"1":1,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    },
                             |    "bar": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    }
                             |  },
                             |  {
                             |    "foo": {
                             |      "a": {"1":1,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    },
                             |    "bar": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    }
                             |  }
                             |]""".stripMargin)

        }

        "multiple value" - {
          val result = Stream
            .emit(jsonString)
            .through(text.utf8Encode)
            .through(tokenParser)
            .through(TokenFilter.downArray.downObject.downField("foo").downObject.downField("a").downObject.removeFields(Set("3", "2")))
            .through(prettyPrinter(JsonStyle.SemiPretty(3)))
            .through(text.utf8Decode)
            .covary[IO]
            .compile
            .foldMonoid
            .unsafeRunSync()

          assert(result == """[
                             |  {
                             |    "foo": {
                             |      "a": {"1":1},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    },
                             |    "bar": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    }
                             |  },
                             |  {
                             |    "foo": {
                             |      "a": {"1":1},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    },
                             |    "bar": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    }
                             |  }
                             |]""".stripMargin)

        }

        "unchunked" - {
          val result = Stream
            .emit(jsonString)
            .through(text.utf8Encode)
            .unchunk
            .through(tokenParser)
            .through(TokenFilter.downArray.downObject.downField("foo").downObject.downField("a").downObject.removeFields(Set("3", "2")))
            .through(prettyPrinter(JsonStyle.SemiPretty(3)))
            .through(text.utf8Decode)
            .covary[IO]
            .compile
            .foldMonoid
            .unsafeRunSync()

          assert(result == """[
                             |  {
                             |    "foo": {
                             |      "a": {"1":1},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    },
                             |    "bar": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    }
                             |  },
                             |  {
                             |    "foo": {
                             |      "a": {"1":1},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    },
                             |    "bar": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    }
                             |  }
                             |]""".stripMargin)

        }
      }

      "inserting fields" - {
        "simple values" - {

          val insertStream = Stream.emits(
            Seq(
              Stream.emit(JsonTrue),
              Stream.emit(JsonFalse)
            ))

          val result = Stream
            .emit(jsonString)
            .through(text.utf8Encode)
            .through(tokenParser)
            .through2(insertStream)(TokenFilter.downArray.downObject.downField("foo").downObject.insertField("c"))
            .through(prettyPrinter(JsonStyle.SemiPretty(3)))
            .through(text.utf8Decode)
            .covary[IO]
            .compile
            .foldMonoid
            .unsafeRunSync()

          assert(result == """[
                             |  {
                             |    "foo": {
                             |      "c": true,
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    },
                             |    "bar": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    }
                             |  },
                             |  {
                             |    "foo": {
                             |      "c": false,
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    },
                             |    "bar": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    }
                             |  }
                             |]""".stripMargin)

        }

        "complex" - {

          val insertStream = Stream.emits(
            Seq(
              Stream.emits(Seq(ObjectStart, ObjectField.fromString("t"), JsonTrue, ObjectEnd)),
              Stream.emits(Seq(ArrayStart, JsonNumber.fromString("10"), JsonNumber.fromString("20"), ArrayEnd))
            ))

          val result = Stream
            .emit(jsonString)
            .through(text.utf8Encode)
            .through(tokenParser)
            .through2(insertStream)(TokenFilter.downArray.downObject.downField("foo").downObject.insertField("c"))
            .through(prettyPrinter(JsonStyle.SemiPretty(3)))
            .through(text.utf8Decode)
            .covary[IO]
            .compile
            .foldMonoid
            .unsafeRunSync()

          assert(result == """[
                             |  {
                             |    "foo": {
                             |      "c": {"t":true},
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    },
                             |    "bar": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    }
                             |  },
                             |  {
                             |    "foo": {
                             |      "c": [10,20],
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    },
                             |    "bar": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    }
                             |  }
                             |]""".stripMargin)

        }

        "early termination" - {

          val insertStream = Stream.emits(
            Seq(
              Stream.emit(JsonTrue)
            ))

          val result = Stream
            .emit(jsonString)
            .through(text.utf8Encode)
            .through(tokenParser)
            .through2(insertStream)(TokenFilter.downArray.downObject.downField("foo").downObject.insertField("c"))
            .through(prettyPrinter(JsonStyle.SemiPretty(3)))
            .through(text.utf8Decode)
            .covary[IO]
            .compile
            .foldMonoid
            .unsafeRunSync()

          assert(result == """[
                             |  {
                             |    "foo": {
                             |      "c": true,
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    },
                             |    "bar": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    }
                             |  },
                             |  {
                             |    "foo": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    },
                             |    "bar": {
                             |      "a": {"1":1,"2":true,"3":3},
                             |      "b": {"1":1,"2":true,"3":3}
                             |    }
                             |  }
                             |]""".stripMargin)

        }
      }
    }

    // for code coverage
    "JsonTokens" - {
      "toString" - {
        assert(JsonString.fromString("foo").toString == "JsonString(\"foo\")")
      }
    }
  }
}
