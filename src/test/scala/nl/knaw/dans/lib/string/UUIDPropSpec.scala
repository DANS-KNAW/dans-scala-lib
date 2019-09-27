package nl.knaw.dans.lib.string

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ EitherValues, Matchers, PropSpec }

class UUIDPropSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with EitherValues {

  property("a valid UUID, converted to a String should be parsed back to the same UUID") {
    forAll(Gen.uuid)(uuid => {
      val uuidString = uuid.toString

      uuidString.toUUID.right.value shouldBe uuid
    })
  }
}
