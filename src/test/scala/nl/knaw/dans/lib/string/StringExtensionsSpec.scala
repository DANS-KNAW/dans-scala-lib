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

import java.util.UUID

import org.scalatest.{ EitherValues, FlatSpec, Matchers, OptionValues }

class StringExtensionsSpec extends FlatSpec with Matchers with OptionValues with EitherValues {

  "isBlank" should "return true when given an empty String" in {
    "".isBlank shouldBe true
  }

  it should "return true when the input is null" in {
    (null: String).isBlank shouldBe true
  }

  it should "return true when given a String with spaces only" in {
    "   ".isBlank shouldBe true
  }

  it should "return true when given a String with tabs, spaces, newlines, etc" in {
    " \t  \r  \t \n  ".isBlank shouldBe true
  }

  it should "return false when given a non-blank String" in {
    "abc".isBlank shouldBe false
  }

  it should "return false when the input contains both blank and non-blank characters" in {
    val input = "ab c "
    input.isBlank shouldBe false
  }

  "toOption" should "return an Option.empty when given an empty String" in {
    "".toOption shouldBe empty
  }

  it should "return an Option.empty when the input is null" in {
    (null: String).toOption shouldBe empty
  }

  it should "return an Option.empty when given a String with spaces only" in {
    "   ".toOption shouldBe empty
  }

  it should "return an Option.empty when given a String with tabs, spaces, newlines, etc" in {
    " \t  \r  \t \n  ".toOption shouldBe empty
  }

  it should "return the original String wrapped in an Option when given a non-blank String" in {
    "abc".toOption.value shouldBe "abc"
  }

  it should "return the original String wrapped in an Option when the input contains both blank and non-blank characters" in {
    val input = "ab c "
    input.toOption.value shouldBe input
  }

  "emptyIfBlank" should "return an empty String when given an empty String" in {
    "".emptyIfBlank shouldBe empty
  }

  it should "return an empty String when the input is null" in {
    (null: String).emptyIfBlank shouldBe empty
  }

  it should "return an empty String when given a String with spaces only" in {
    "   ".emptyIfBlank shouldBe empty
  }

  it should "return an empty String when given a String with tabs, spaces, newlines, etc" in {
    " \t  \r  \t \n  ".emptyIfBlank shouldBe empty
  }

  it should "return the original String when given a non-blank String" in {
    "abc".emptyIfBlank shouldBe "abc"
  }

  it should "return the original String when the input contains both blank and non-blank characters" in {
    val input = "ab c "
    input.emptyIfBlank shouldBe input
  }

  "toUUID" should "return a UUID on a valid input" in {
    val input = "12341234-1234-1234-1234-123412341234"
    val expectedOutput = UUID.fromString(input)
    
    input.toUUID.right.value shouldBe expectedOutput
  }
  
  it should "return an error on an invalid input" in {
    val input = "xyz"
    
    input.toUUID.left.value shouldBe UUIDError(input)
  }

  it should "return an error on an input that is valid according to Java's UUID parser, but does not conform to the pattern of a UUID" in {
    val input = "1-2-3-4-5"
    val javaOutput = UUID.fromString(input)

    // this is what the Java parser does, but this is not what we want!
    javaOutput.toString shouldBe "00000001-0002-0003-0004-000000000005"
    
    // this is what we would expect for an not-wellformed UUID
    input.toUUID.left.value shouldBe UUIDError(input)
  }
}
