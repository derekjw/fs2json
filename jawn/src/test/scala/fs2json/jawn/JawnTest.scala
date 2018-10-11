package fs2json.jawn

import fs2._
import fs2json.{UTestScalaCheck, jawn, tokenParser}
import io.circe.Json
import io.circe.jawn.CirceSupportParser.facade
import io.circe.testing.instances._
import org.scalacheck.Prop.forAll
import utest._

object JawnTest extends TestSuite with UTestScalaCheck {
  val tests = Tests {
    def roundTrip(json: Json): Vector[Json] =
      Stream
        .emit(json.noSpaces)
        .through(text.utf8Encode)
        .through(tokenParser)
        .through(jawn.valueStream[Pure, Json](facade))
        .toVector

    "for all json" - {
      forAll { (json: Json) =>
        roundTrip(json) == Vector(json)
      }.checkUTest()
    }
  }
}
