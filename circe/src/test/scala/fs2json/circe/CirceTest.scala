package fs2json.circe

import fs2._
import fs2json.UTestScalaCheck
import utest._
import io.circe.Json
import org.scalacheck.Prop.forAll
import io.circe.testing.instances._

object CirceTest extends TestSuite with UTestScalaCheck {
  val tests = Tests {
    def roundTrip(json: Json): Vector[Json] =
      Stream.emit(json)
        .through(tokenStream)
        .through(valueStream)
        .toVector

    "for all json" - {
      forAll { (json: Json) =>
        roundTrip(json) == Vector(json)
      }.checkUTest()
    }
  }
}