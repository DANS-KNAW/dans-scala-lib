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
package nl.knaw.dans.lib.encode

import java.nio.file.Paths

import org.scalatest.{ FlatSpec, Matchers, OptionValues }

class EncodingSpec extends FlatSpec with Matchers with OptionValues {

  "escapePath" should "return a string without any escaped characters when the path segments contain only alphanumeric characters or a '_' character" in {
    val s = "abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/0123456789/_"
    val path = Paths.get(s)
    path.escapePath shouldBe s
  }

  it should "return a string where all the characters, except alphanumeric characters and characters '_' and '/', are escaped" in {
    val path = Paths.get("abc/def_12345/! @#$%^&*()-/[]{}'|`~,.<>?/好棒啊，汉语也可以")
    path.escapePath shouldBe "abc/def_12345/%21%20%40%23%24%25%5E%26%2A%28%29%2D/%5B%5D%7B%7D%27%7C%60%7E%2C%2E%3C%3E%3F/%E5%A5%BD%E6%A3%92%E5%95%8A%EF%BC%8C%E6%B1%89%E8%AF%AD%E4%B9%9F%E5%8F%AF%E4%BB%A5"
  }

  "escapeString" should "return a string without any escaped characters when the input string contains only alphanumeric characters or a '_' character" in {
    val s = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
    s.escapeString shouldBe s
  }

  it should "return a string where all the characters are escaped when the input string does not contain alphanumeric characters or a '_' character" in {
    val s = "! @#$%^&*()-[]{}'|`~,.<>/?好棒啊，汉语也可以"
    s.escapeString shouldBe "%21%20%40%23%24%25%5E%26%2A%28%29%2D%5B%5D%7B%7D%27%7C%60%7E%2C%2E%3C%3E%2F%3F%E5%A5%BD%E6%A3%92%E5%95%8A%EF%BC%8C%E6%B1%89%E8%AF%AD%E4%B9%9F%E5%8F%AF%E4%BB%A5"
  }

  it should "return also character '/' escaped" in {
    val s = "12345/dans"
    s.escapeString shouldBe "12345%2Fdans"
  }

}
