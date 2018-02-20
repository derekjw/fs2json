# fs2json

[![Maven Central](https://img.shields.io/maven-central/v/com.github.derekjw/fs2json-core_2.12.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.derekjw/fs2json-core_2.12)
[![Travis Build Status](https://travis-ci.org/derekjw/fs2json.svg?branch=master)](https://travis-ci.org/derekjw/fs2json)
[![Coverage status](https://img.shields.io/codecov/c/github/derekjw/fs2json/master.svg)](https://codecov.io/github/derekjw/fs2json)
[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

### About

fs2Json is a library to create a stream of json tokens and manipulate them.
This allows doing basic operations on large json documents without loading the entire file into memory at one time.
This library was created due to the need of manipulating json documents several gigabytes in size, and is probably not as useful for very small documents.

A stream of bytes are parsed and converted into a stream of token representing the structure of a json document, including object start/end, array start/end, object fields, and the individual json values.

Filters are included to add or remove fields from the stream.

The token stream can be fed into a Jawn parser to transform into some Json AST, and a circe tokenizer for transforming a circe Json document to a stream of tokens.

Until this library hits version 1.0, breaking changes are expected and bugs are likely. There has also been little testing against malformed Json.

### SBT
```
libraryDependencies += "com.github.derekjw" %% "fs2json-core" % "0.6.1"

// optional jawn library
libraryDependencies += "com.github.derekjw" %% "fs2json-jawn" % "0.6.1"

// optional circe library
libraryDependencies += "com.github.derekjw" %% "fs2json-circe" % "0.6.1"
```

### Examples

```scala
import fs2._
import fs2json._
import cats.effect.IO

val jsonString =
"""[
  {"foo" : {
    "a": { "1": 1, "2" : true, "3" : 3 },
    "b": { "1": 1, "2" : true, "3" : 3 },
  },
  "bar" : {
    "a": { "1": 1, "2" : true, "3" : 3 },
    "b": { "1": 1, "2" : true, "3" : 3 },
  }},
  {"foo" : {
    "a": { "1": 1, "2" : true, "3" : 3 },
    "b": { "1": 1, "2" : true, "3" : 3 },
  },
  "bar" : {
    "a": { "1": 1, "2" : true, "3" : 3 },
    "b": { "1": 1, "2" : true, "3" : 3 },
  }}
]"""

// Field Removal
val removalResult = Stream
    .emit(jsonString)
    .through(text.utf8Encode) // turn json document to a byte stream
    .through(tokenParser) // parse the stream to json tokens
    .through(TokenFilter.downArray.downObject.downField("foo").downObject.removeField("a")) // remove "foo"."a"
    .through(prettyPrinter(JsonStyle.SemiPretty(3))) // print the token stream to a byte stream
    .through(text.utf8Decode) // convert byte stream to strings
    .covary[IO]
    .compile
    .foldMonoid
    .unsafeRunSync()

assert(removalResult == """[
  {
    "foo": {
      "b": {"1":1,"2":true,"3":3}
    },
    "bar": {
      "a": {"1":1,"2":true,"3":3},
      "b": {"1":1,"2":true,"3":3}
    }
  },
  {
    "foo": {
      "b": {"1":1,"2":true,"3":3}
    },
    "bar": {
      "a": {"1":1,"2":true,"3":3},
      "b": {"1":1,"2":true,"3":3}
    }
  }
]""")

// Field Insertion
val insertStream = Stream.emits(Seq(
  Stream.emit(JsonTrue),
  Stream.emit(JsonFalse)
))


val insertResult = Stream
  .emit(jsonString)
  .through(text.utf8Encode)
  .through(tokenParser)
  .through2(insertStream)(TokenFilter.downArray.downObject.downField("foo").downObject.insertField("c")) // insert field "c" using insertStream
  .through(prettyPrinter(JsonStyle.SemiPretty(3)))
  .through(text.utf8Decode)
  .covary[IO]
  .compile
  .foldMonoid
  .unsafeRunSync()

assert(result ==
"""[
  {
    "foo": {
      "c": true,
      "a": {"1":1,"2":true,"3":3},
      "b": {"1":1,"2":true,"3":3}
    },
    "bar": {
      "a": {"1":1,"2":true,"3":3},
      "b": {"1":1,"2":true,"3":3}
    }
  },
  {
    "foo": {
      "c": false,
      "a": {"1":1,"2":true,"3":3},
      "b": {"1":1,"2":true,"3":3}
    },
    "bar": {
      "a": {"1":1,"2":true,"3":3},
      "b": {"1":1,"2":true,"3":3}
    }
  }
]""")

```