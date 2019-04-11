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

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

class MaskerSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {

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

  "formatRemoteAddress" should "properly mask" in {
    val remoteAdressExamples = Table(("plainAddress", "maskedAddress"),
      ("129.144.52.38", "129.**.**.**"), // IPv4

      // https://docs.oracle.com/javase/9/docs/api/java/net/Inet6Address.html
      ("1080:0:0:0:8:800:200C:417A", "1080:0:0:0:8:**:**:**"), // preferred
      ("1080:0:0:0:8:800:200C:417A", "1080:0:0:0:8:**:**:**"), // preferred
      ("1080::8:800:200C:417A", "1080::8:**:**:**"), // suppressed zero sequences
      ("::FFFF:129.144.52.38", "::FFFF:129.**.**.**"), // mixed IPv4/IPv6
      ("::129.144.52.38", "::129.**.**.**"), // mixed IPv4/IPv6
      ("::FFFF:1.2.3", "::FFFF:1.2.3"), // invalid
      ("::FFFF:4.5", "::FFFF:4.5"), // invalid
      ("::6.7.8", "::6.7.8"), // invalid
      ("::9.10", "::9.10"), // invalid
      ("::FFFF:123", "::FFFF:**"), // unconventional representation of ::255.255.0.123
      ("::255.255.0.123", "::255.**.**.**"),
      ("0:0:0:0:0:0:0:123", "0:0:0:0:0:**:**:**"),

      // https://en.wikipedia.org/wiki/Localhost
      ("127.0.0.1", "127.0.0.1"),
      ("::1", "::1"),

      // duplicated examples from different referenced sources are kept as comment for documentation

      // https://www.ietf.org/rfc/rfc3513.txt
      ("FEDC:BA98:7654:3210:FEDC:BA98:7654:3210", "FEDC:BA98:7654:3210:FEDC:**:**:**"), // 2.2.1 example of preferred format
      //("1080:0:0:0:8:800:200C:417A", "???"), // 2.2.1 example of preferred format
      ("0:0:0:0:0:0:0:1", "0:0:0:0:0:0:0:1"), // 2.2.2 long version of ::1 (loopback/localhost)
      ("0:0:0:0:0:0:0:0", "0:0:0:0:0:0:0:0"), // 2.2.2 long version of unspecified address
      ("::", "::"), // 2.2.2 short version of unspecified address
      //("1080:0:0:0:8:800:200C:417A", ""), // 2.2.2 long unicast
      ("FF01:0:0:0:0:0:0:101", "FF01:0:0:0:0:**:**:**"), // 2.2.2 long multicast
      //("1080::8:800:200C:417A", "???"), // 2.2.2 short unicast
      ("FF01::101", "FF01::**"), // 2.2.2 short multicast
      ("0:0:0:0:0:0:13.1.68.3", "0:0:0:0:0:0:13.**.**.**"), // 2.2.3 long mixed IPv4/IPv6 (short between oracle examples)
      ("0:0:0:0:0:FFFF:129.144.52.38", "0:0:0:0:0:FFFF:129.**.**.**"), // 2.2.3 long mixed IPv4/IPv6 (short between oracle examples)

      // https://www.tutorialspoint.com/ipv6/ipv6_address_types.htm
      // composition of IPv6 Unicast:
      // * 48 bits (3 hex blocks) Global Routing Prefix
      // * 16 bits (1 hex block) Subnet ID
      // * 64 bits (2 hex blocks) Interface ID (possibly derived from a globally unique Mac address)

      // https://en.wikipedia.org/wiki/Reserved_IP_addresses
      // https://en.wikipedia.org/wiki/IP_address
      // TODO more special cases?

      // self invented cases with more or less randomly suppressed zero blocks
      ("FEDC:BA98:7654:3210:FEDC::7654:3210", "FEDC:BA98:7654:3210:FEDC:**:**:**"),
      ("FEDC:BA98:7654:3210:FEDC:BA98::3210", "FEDC:BA98:7654:3210:FEDC:**:**:**"),
      // the next ones are ambiguous: not clear which block is/are non-zero
      ("BA98::3210::", "BA98::**::"),
      ("::BA98::3210", "::BA98::**"),
      ("::BA98::3210::", "::BA98::**::"),
      ("::3210::", ":**:**:**"),
    )
    forEvery(remoteAdressExamples) { (plainAddress: String, maskedAddress: String) =>
      Masker.formatRemoteAddress(plainAddress) shouldBe maskedAddress
    }
  }
}
