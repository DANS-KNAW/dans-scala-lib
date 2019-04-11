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
import org.scalatra._

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
  
  val actionResult = Ok(body = "hello world", headers = Map("some" -> "header"))

  "formatResponseLog" should "return a formatted log String for the response" in {
    val testServlet: TestServlet = new TestServlet()
    testServlet.formatResponseLog(actionResult) shouldBe
      "response GET http://does.not.exist.dans.knaw.nl returned status=200; headers=[Set-Cookie -> [scentry.auth.default.user=abc456.pq.xy], REMOTE_USER -> [somebody], Expires -> [Thu, 01 Jan 1970 00:00:00 GMT], some -> [header]]"
  }

  it should "mask everything when using the MaskedResponseLogFormatter" in {
    val testServlet: TestServlet = new TestServlet() with MaskedResponseLogFormatter
    testServlet.formatResponseLog(actionResult) shouldBe
      "response GET http://does.not.exist.dans.knaw.nl returned status=200; headers=[Set-Cookie -> [scentry.auth.default.user=****], REMOTE_USER -> [*****], Expires -> [Thu, 01 Jan 1970 00:00:00 GMT], some -> [header]]"
  }

  it should "add the response body when using LogResponseBodyAlways" in {
    val testServlet: TestServlet = new TestServlet() with LogResponseBodyAlways {
      // make formatResponseLog public again
      override def formatResponseLog(actionResult: ActionResult): String = super.formatResponseLog(actionResult)
    }
    testServlet.formatResponseLog(actionResult) shouldBe
      "response GET http://does.not.exist.dans.knaw.nl returned status=200; headers=[Set-Cookie -> [scentry.auth.default.user=abc456.pq.xy], REMOTE_USER -> [somebody], Expires -> [Thu, 01 Jan 1970 00:00:00 GMT], some -> [header]]; body=[hello world]"
  }

  it should "add an empty response body when using LogResponseBodyAlways with a blank body" in {
    val testServlet: TestServlet = new TestServlet() with LogResponseBodyAlways {
      // make formatResponseLog public again
      override def formatResponseLog(actionResult: ActionResult): String = super.formatResponseLog(actionResult)
    }
    testServlet.formatResponseLog(Ok("   \t   \n   ")) shouldBe
      "response GET http://does.not.exist.dans.knaw.nl returned status=200; headers=[Set-Cookie -> [scentry.auth.default.user=abc456.pq.xy], REMOTE_USER -> [somebody], Expires -> [Thu, 01 Jan 1970 00:00:00 GMT]]; body=[]"
  }

  it should "add no response body when using LogResponseBodyAlways with response without a body" in {
    val testServlet: TestServlet = new TestServlet() with LogResponseBodyAlways {
      // make formatResponseLog public again
      override def formatResponseLog(actionResult: ActionResult): String = super.formatResponseLog(actionResult)
    }
    testServlet.formatResponseLog(NoContent()) shouldBe
      "response GET http://does.not.exist.dans.knaw.nl returned status=204; headers=[Set-Cookie -> [scentry.auth.default.user=abc456.pq.xy], REMOTE_USER -> [somebody], Expires -> [Thu, 01 Jan 1970 00:00:00 GMT]]"
  }

  it should "add the response body when using LogResponseBodyOnError with an error response" in {
    val testServlet: TestServlet = new TestServlet() with LogResponseBodyOnError {
      // make formatResponseLog public again
      override def formatResponseLog(actionResult: ActionResult): String = super.formatResponseLog(actionResult)
    }
    testServlet.formatResponseLog(NotAcceptable("this is unacceptable behaviour")) shouldBe
      "response GET http://does.not.exist.dans.knaw.nl returned status=406; headers=[Set-Cookie -> [scentry.auth.default.user=abc456.pq.xy], REMOTE_USER -> [somebody], Expires -> [Thu, 01 Jan 1970 00:00:00 GMT]]; body=[this is unacceptable behaviour]"
  }

  it should "not add the response body when using LogResponseBodyOnError with a successful response" in {
    val testServlet: TestServlet = new TestServlet() with LogResponseBodyOnError {
      // make formatResponseLog public again
      override def formatResponseLog(actionResult: ActionResult): String = super.formatResponseLog(actionResult)
    }
    testServlet.formatResponseLog(actionResult) shouldBe
      "response GET http://does.not.exist.dans.knaw.nl returned status=200; headers=[Set-Cookie -> [scentry.auth.default.user=abc456.pq.xy], REMOTE_USER -> [somebody], Expires -> [Thu, 01 Jan 1970 00:00:00 GMT], some -> [header]]"
  }

  it should "add an empty response body when using LogResponseBodyOnError with a blank body" in {
    val testServlet: TestServlet = new TestServlet() with LogResponseBodyOnError {
      // make formatResponseLog public again
      override def formatResponseLog(actionResult: ActionResult): String = super.formatResponseLog(actionResult)
    }
    testServlet.formatResponseLog(InternalServerError("   \t   \n   ")) shouldBe
      "response GET http://does.not.exist.dans.knaw.nl returned status=500; headers=[Set-Cookie -> [scentry.auth.default.user=abc456.pq.xy], REMOTE_USER -> [somebody], Expires -> [Thu, 01 Jan 1970 00:00:00 GMT]]; body=[]"
  }

  it should "add no response body when using LogResponseBodyOnError with response without a body" in {
    val testServlet: TestServlet = new TestServlet() with LogResponseBodyOnError {
      // make formatResponseLog public again
      override def formatResponseLog(actionResult: ActionResult): String = super.formatResponseLog(actionResult)
    }
    testServlet.formatResponseLog(NotFound()) shouldBe
      "response GET http://does.not.exist.dans.knaw.nl returned status=404; headers=[Set-Cookie -> [scentry.auth.default.user=abc456.pq.xy], REMOTE_USER -> [somebody], Expires -> [Thu, 01 Jan 1970 00:00:00 GMT]]"
  }
}
