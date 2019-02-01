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

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import nl.knaw.dans.lib.logging.servlet.masked.MaskedResponseLogFormatter
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ FlatSpec, Matchers }
import org.scalatra.Ok

import scala.collection.JavaConverters._

class ResponseLogFormatterSpec extends FlatSpec with Matchers with MockFactory with TestServletFixture {

  private val mockHeaders: HeaderMap = Map(
    "Set-Cookie" -> Seq("scentry.auth.default.user=abc456.pq.xy"),
    "REMOTE_USER" -> Seq("somebody"),
    "Expires" -> Seq("Thu, 01 Jan 1970 00:00:00 GMT"), // a date in the past means no cache for the returned content
  )

  override protected def mockRequest: HttpServletRequest = {
    val req = super.mockRequest
    (() => req.getMethod) expects() returning "GET" anyNumberOfTimes()
    (() => req.getRequestURL) expects() returning new StringBuffer("http://does.not.exist.dans.knaw.nl") anyNumberOfTimes()
    req
  }

  override protected def mockResponse: HttpServletResponse = {
    val response = super.mockResponse
    val headers = mockHeaders

    headers.foreach { case (key: String, values: Seq[String]) =>
      response.getHeaders _ expects key anyNumberOfTimes() returning values.asJava
    }
    (() => response.getHeaderNames) expects() anyNumberOfTimes() returning headers.keys.toSeq.asJava
    response
  }

  "formatResponseLog" should "return a formatted log String for the response" in {
    new TestServlet().formatResponseLog(Ok(headers = Map("some" -> "header"))) shouldBe
      "GET http://does.not.exist.dans.knaw.nl returned status=200; authHeaders=[Set-Cookie -> [scentry.auth.default.user=abc456.pq.xy], REMOTE_USER -> [somebody], Expires -> [Thu, 01 Jan 1970 00:00:00 GMT]]; actionHeaders=[some -> header]"
  }

  it should "mask everything when using the MaskedResponseLogFormatter" in {
    (new TestServlet() with MaskedResponseLogFormatter).formatResponseLog(Ok(headers = Map("some" -> "header"))) shouldBe
      "GET http://does.not.exist.dans.knaw.nl returned status=200; authHeaders=[Set-Cookie -> [scentry.auth.default.user=****.****.****], REMOTE_USER -> [*****], Expires -> [Thu, 01 Jan 1970 00:00:00 GMT]]; actionHeaders=[some -> header]"
  }
}
