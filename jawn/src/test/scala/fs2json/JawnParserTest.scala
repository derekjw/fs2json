package fs2json

import fs2._
import utest._
import io.circe.Json
import io.circe.jawn.CirceSupportParser.facade
import org.scalacheck.Prop.forAll
import io.circe.testing.instances._

object JawnParserTest extends TestSuite with UTestScalaCheck {
  val tests = Tests {
    def roundTrip(json: Json): Vector[Json] =
      Stream.emit(json.noSpaces).through(text.utf8Encode)
        .through(tokenParser)
        .through(jawn.valueStream)
        .toVector

    "for all json" - {
      forAll { (json: Json) =>
        roundTrip(json) == Vector(json)
      }.checkUTest()
    }
  }
}