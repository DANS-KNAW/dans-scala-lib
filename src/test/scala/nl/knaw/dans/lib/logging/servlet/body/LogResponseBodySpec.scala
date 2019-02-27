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
package nl.knaw.dans.lib.logging.servlet.body

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import nl.knaw.dans.lib.logging.servlet.TestServletFixture
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ FlatSpec, Matchers }
import org.scalatra.{ ActionResult, Ok }

import scala.collection.JavaConverters._

class LogResponseBodySpec extends FlatSpec with Matchers with MockFactory with TestServletFixture {
  
  class LogResponseBodyTestServlet extends TestServlet(request=mockRequest, response = mockResponse)
    with LogResponseBody {
    def shouldLogResponseBody(response: HttpServletResponse): Boolean = true

    override def formatResponseLog(actionResult: ActionResult): String = super.formatResponseLog(actionResult)
  }

  override protected def mockRequest: HttpServletRequest = {
    val req = super.mockRequest
    (() => req.getMethod) expects() returning "GET" anyNumberOfTimes()
    (() => req.getRequestURL) expects() returning new StringBuffer("http://does.not.exist.dans.knaw.nl") anyNumberOfTimes()
    req
  }

  override protected def mockResponse: HttpServletResponse = {
    val response = super.mockResponse

    (() => response.getHeaderNames) expects() anyNumberOfTimes() returning Seq.empty[String].asJava
    response
  }

  "formatResponseLog" should "return a formatted log String for the response including the body" in {
    new LogResponseBodyTestServlet().formatResponseLog(Ok(body = "hello world")) shouldBe
      "response GET http://does.not.exist.dans.knaw.nl returned status=200; headers=[]; body=[hello world]"
  }
}
