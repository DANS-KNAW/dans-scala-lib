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
package nl.knaw.dans.lib.logging.servlet

import javax.servlet.http.HttpServletRequest
import nl.knaw.dans.lib.logging.servlet.masked.MaskedRequestLogFormatter
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ FlatSpec, Matchers }
import org.scalatra.MultiParams

import scala.collection.JavaConverters._

class RequestLogFormatterSpec extends FlatSpec with Matchers with MockFactory with TestServletFixture {

  override protected val mockParams: MultiParams = Map(
    "foo" -> Seq("bar"),
  )

  private val mockHeaders: HeaderMap = Map(
    "cookie" -> Seq("scentry.auth.default.user=abc456.pq.xy"),
    "HTTP_AUTHORIZATION" -> Seq("basic 123x_"),
    "foo" -> Seq("bar"),
  )

  override protected def mockRequest: HttpServletRequest = {
    val request = super.mockRequest
    val headers = mockHeaders

    for ((key, values) <- headers) {
      request.getHeaders _ expects key anyNumberOfTimes() returning values.iterator.asJavaEnumeration
    }

    (() => request.getMethod) expects() anyNumberOfTimes() returning "GET"
    (() => request.getRequestURL) expects() anyNumberOfTimes() returning new StringBuffer("http://does.not.exist.dans.knaw.nl")
    (() => request.getRemoteAddr) expects() anyNumberOfTimes() returning "12.34.56.78"
    (() => request.getHeaderNames) expects() anyNumberOfTimes() returning headers.keys.iterator.asJavaEnumeration

    request
  }

  "formatRequestLog" should "return a formatted log String for the request" in {
    new TestServlet().formatRequestLog shouldBe
      "request GET http://does.not.exist.dans.knaw.nl remote=12.34.56.78; params=[foo -> [bar]]; headers=[cookie -> [scentry.auth.default.user=abc456.pq.xy], HTTP_AUTHORIZATION -> [basic 123x_], foo -> [bar]]"
  }

  it should "mask everything when using the MaskedRequestLogFormatter" in {
    (new TestServlet() with MaskedRequestLogFormatter).formatRequestLog shouldBe
      "request GET http://does.not.exist.dans.knaw.nl remote=**.**.**.78; params=[foo -> [bar]]; headers=[cookie -> [scentry.auth.default.user=****.****.****], HTTP_AUTHORIZATION -> [basic *****], foo -> [bar]]"
  }
}
