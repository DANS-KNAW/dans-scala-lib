/**
 * Copyright (C) 2016 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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
