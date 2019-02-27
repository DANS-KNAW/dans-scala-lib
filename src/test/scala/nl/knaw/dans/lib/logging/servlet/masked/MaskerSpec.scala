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
package nl.knaw.dans.lib.logging.servlet.masked

import org.scalatest.{ FlatSpec, Matchers }

class MaskerSpec extends FlatSpec with Matchers {

  private val cookieKey = "scentry.auth.default.user"
  private val cookieValue = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjE1NDcyMDc2MjksImlhdCI6MTU0NzIwNDAyOSwidWlkIjoidXNlcjAwMSJ9.UH3bMyWaUimn0ctbEcThh4hx5LlvYJ61kfvzU4O5JPI"
  private val cookie = s"$cookieKey=$cookieValue"

  "formatCookie" should "replaces cookie value with ****" in {
    Masker.formatCookie(cookie) shouldBe s"$cookieKey=****.****.****"
  }

  it should "also replace = sign in the cookie value" in {
    val value = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjE1NDcyMDc2MjksImlhdCI6MTU0NzIwNDAyOSwidWlkIjoidXNlcjAwMSJ9.UH3bMyWaUi=mn0ctbEcThh4hx5LlvYJ61kfvzU4O5JPI"
    val cookie = s"$cookieKey=$value"
    Masker.formatCookie(cookie) shouldBe s"$cookieKey=****.****.****"
  }

  "formatRemoteAddress" should "replace the first part of an IPv4 remote address" in {
    Masker.formatRemoteAddress("127.0.0.1") shouldBe "**.**.**.1"
  }

  "formatCookieHeader" should "format a cookie with given header name" in {
    val cookieName = "my-cookie"
    Masker.formatCookieHeader(cookieName)(Masker.formatCookie)(cookieName -> Seq(cookie)) shouldBe
      cookieName -> Seq(s"$cookieKey=****.****.****")
  }

  it should "format a cookie with given header name (after lowercasing)" in {
    val cookieName = "my-cookie"
    Masker.formatCookieHeader(cookieName)(Masker.formatCookie)(cookieName.toUpperCase -> Seq(cookie)) shouldBe
      cookieName.toUpperCase -> Seq(s"$cookieKey=****.****.****")
  }

  it should "not format a header with another name than the given one" in {
    Masker.formatCookieHeader("my-cookie")(Masker.formatCookie)("other-header" -> Seq("some value")) shouldBe
      "other-header" -> Seq("some value")
  }

  "formatAuthorizationHeader" should "format authorization" in {
    val headerKey = "basic-authorization"
    Masker.formatAuthorizationHeader(headerKey -> Seq("basic some-value")) shouldBe
      headerKey -> Seq("basic *****")
  }

  it should "format authorization (after lowercasing)" in {
    val headerKey = "basic-authorization"
    Masker.formatAuthorizationHeader(headerKey.toUpperCase -> Seq("basic some-value")) shouldBe
      headerKey.toUpperCase -> Seq("basic *****")
  }

  it should "format authorization where a space is in the latter part of the value" in {
    val headerKey = "basic-authorization"
    Masker.formatAuthorizationHeader(headerKey -> Seq("basic some value")) shouldBe
      headerKey -> Seq("basic *****")
  }

  it should "not format a header with another name than the given one" in {
    Masker.formatAuthorizationHeader("other-header" -> Seq("some value")) shouldBe
      "other-header" -> Seq("some value")
  }

  "formatRemoteUserHeader" should "format remote user" in {
    val headerKey = "remote_user"
    Masker.formatRemoteUserHeader(headerKey -> Seq("my-name")) shouldBe
      headerKey -> Seq("*****")
  }

  it should "format remote user (after lowercasing)" in {
    val headerKey = "remote_user"
    Masker.formatRemoteUserHeader(headerKey.toUpperCase -> Seq("my-name")) shouldBe
      headerKey.toUpperCase -> Seq("*****")
  }

  it should "not format a header with another name than the given one" in {
    Masker.formatRemoteUserHeader("other-header" -> Seq("some value")) shouldBe
      "other-header" -> Seq("some value")
  }
}
