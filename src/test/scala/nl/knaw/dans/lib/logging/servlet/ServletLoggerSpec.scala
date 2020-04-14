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

import com.typesafe.scalalogging.Logger
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatra._
import org.scalatra.test.EmbeddedJettyContainer
import org.scalatra.test.scalatest.ScalatraSuite
import org.slf4j.{ Logger => Underlying }

import scala.concurrent.{ ExecutionContext, Future }

class ServletLoggerSpec extends AnyFlatSpec with Matchers with MockFactory with EmbeddedJettyContainer with ScalatraSuite {

  private val mockedLogger = mock[Underlying]

  private trait TestLogger extends ServletLogger with PlainLogFormatter with LogResponseBodyOnError {
    this: ScalatraBase =>

    override protected val logger: Logger = Logger(mockedLogger)
  }

  private class TestServlet() extends ScalatraServlet with FutureSupport with TestLogger {
    implicit protected val executor: ExecutionContext = ExecutionContext.global

    get("/") {
      contentType = "text/plain"
      Ok("How y'all doin'?")
    }

    get("/:input") {
      contentType = "text/plain"
      val input = params("input")
      Ok(s"I received $input")
    }

    get("/unit") {
      Forbidden()
    }

    // POST /create?input=...
    post("/create") {
      val input = params("input")
      Ok(s"I received $input")
    }

    get("/halted") {
      halt(Unauthorized(body = "invalid credentials", headers = Map("foo" -> "bar")))
    }

    get("/error") {
      throw new IllegalArgumentException("this is an error!!!")
    }

    get("/future") {
      Future {
        Ok("this is a message from the future")
      }
    }
  }

  private val testLoggerPath = "/testLoggerPath"

  addServlet(new TestServlet(), testLoggerPath)

  private def infoLogEnabled(times: Int) = {
    (() => mockedLogger.isInfoEnabled()) expects() repeated times returning true
  }

  private def warnLogEnabled(times: Int) = {
    (() => mockedLogger.isWarnEnabled()) expects() repeated times returning true
  }

  private def expectInfoLogMessage(predicates: (String => Boolean)*) = {
    (mockedLogger.info(_: String)) expects where {
      s: String => predicates forall (_ (s))
    } once()
  }

  private def expectAnyInfoLog(times: Int = 1) = {
    (mockedLogger.info(_: String)) expects * repeated times
  }

  private def expectWarnLogMessage(predicates: (String => Boolean)*) = {
    (mockedLogger.warn(_: String, _: Throwable)) expects where {
      (s, _) => predicates forall (_ (s))
    }
  }

  "TestServlet" should "call the logRequest on an incoming request" in {
    val serverPort = localPort.fold("None")(_.toString)

    infoLogEnabled(2)
    expectInfoLogMessage(
      _ startsWith s"request GET http://localhost:$serverPort$testLoggerPath",
      _ contains "remote=127.0.0.1"
    )
    expectAnyInfoLog()

    get(testLoggerPath) {
      body shouldBe "How y'all doin'?"
      status shouldBe 200
    }
  }

  it should "call the logResponse on sending a response" in {
    val port = localPort.fold("None")(_.toString)

    infoLogEnabled(2)
    expectInfoLogMessage(
      _ startsWith s"response GET http://localhost:$port$testLoggerPath returned status=200; headers=[",
      _.toLowerCase contains "content-type -> [text/plain;charset=utf-8]"
    )
    expectAnyInfoLog()

    get(testLoggerPath) {
      body shouldBe "How y'all doin'?"
      status shouldBe 200
    }
  }

  it should "log the parameter given in the URL" in {
    val serverPort = localPort.fold("None")(_.toString)
    val input = "my-input-string"

    infoLogEnabled(2)
    expectInfoLogMessage(
      _ startsWith s"request GET http://localhost:$serverPort$testLoggerPath/$input",
      _ contains "remote=127.0.0.1"
    )
    expectAnyInfoLog()

    get(s"$testLoggerPath/$input") {
      body shouldBe s"I received $input"
      status shouldBe 200
    }
  }

  it should "deal appropriately with Unit content" in {
    val serverPort = localPort.fold("None")(_.toString)

    infoLogEnabled(2)
    expectInfoLogMessage(
      _ startsWith s"request GET http://localhost:$serverPort$testLoggerPath/unit",
      _ contains "remote=127.0.0.1"
    )
    expectInfoLogMessage(
      _ startsWith s"response GET http://localhost:$serverPort$testLoggerPath/unit returned status=403",
      _.toLowerCase contains "content-type -> [text/html;charset=utf-8]",
      s => !(s contains "; body=[") // because of Unit response in error
    )

    get(s"$testLoggerPath/unit") {
      body shouldBe empty
      status shouldBe 403
    }
  }

  it should "log the form parameter given in the URL" in {
    val serverPort = localPort.fold("None")(_.toString)
    val input = "my-input-string"

    infoLogEnabled(2)
    expectInfoLogMessage(
      _ startsWith s"request POST http://localhost:$serverPort$testLoggerPath/create",
      _ contains "remote=127.0.0.1",
      _ contains s"params=[input -> [$input]",
    )
    expectAnyInfoLog()

    // POST http://localhost:$serverPort/$testLoggerPath/create?input=$input
    post(s"$testLoggerPath/create", "input" -> input) {
      body shouldBe s"I received $input"
      status shouldBe 200
    }
  }

  it should "log when halt is called" in {
    val serverPort = localPort.fold("None")(_.toString)

    infoLogEnabled(2)
    expectInfoLogMessage(
      _ startsWith s"request GET http://localhost:$serverPort$testLoggerPath/halted",
      _ contains "remote=127.0.0.1",
    )
    expectInfoLogMessage(
      _ startsWith s"response GET http://localhost:$serverPort$testLoggerPath/halted returned status=401",
      _.toLowerCase contains "content-type -> [text/plain;charset=utf-8]",
      _.toLowerCase contains "foo -> [bar]",
      _ contains "; body=[invalid credentials]",
    )

    get(s"$testLoggerPath/halted") {
      body shouldBe s"invalid credentials"
      status shouldBe 401
    }
  }

  it should "call the renderUncaughtException when an exception is thrown in the servlet route" in {
    val serverPort = localPort.fold("None")(_.toString)

    infoLogEnabled(1)
    warnLogEnabled(1)
    expectInfoLogMessage(
      _ startsWith s"request GET http://localhost:$serverPort$testLoggerPath/error",
      _ contains "remote=127.0.0.1",
    )
    expectWarnLogMessage(
      _ == s"response GET http://localhost:$serverPort$testLoggerPath/error resulted in an uncaught exception: this is an error!!!"
    )

    get(s"$testLoggerPath/error") {
      body should startWith("java.lang.IllegalArgumentException: this is an error!!!")
      status shouldBe 500
    }
  }

  it should "not print a response twice when a Future is returned" in {
    val serverPort = localPort.fold("None")(_.toString)

    infoLogEnabled(2)
    expectInfoLogMessage(
      _ startsWith s"request GET http://localhost:$serverPort$testLoggerPath/future",
      _ contains "remote=127.0.0.1"
    )
    expectInfoLogMessage(
      _ startsWith s"response GET http://localhost:$serverPort$testLoggerPath/future returned status=200",
      _.toLowerCase contains "content-type -> [text/plain;charset=utf-8]",
    )

    get(s"$testLoggerPath/future") {
      body shouldBe "this is a message from the future"
      status shouldBe 200
    }
  }

  it should "log when a non-existing route is called" in {
    val serverPort = localPort.fold("None")(_.toString)

    infoLogEnabled(2)
    expectInfoLogMessage(
      _ startsWith s"request GET http://localhost:$serverPort$testLoggerPath/not-existing/",
      _ contains "remote=127.0.0.1",
    )
    expectInfoLogMessage(
      _ startsWith s"response GET http://localhost:$serverPort$testLoggerPath/not-existing/ returned status=404",
      _.toLowerCase contains "content-type -> [text/html;charset=utf-8]",
      s => !(s contains "; body=["), // because the body cannot be picked up due to the implementation of Scalatra
    )

    get(s"$testLoggerPath/not-existing/") {
      body should startWith("""Requesting "GET /not-existing/" on servlet "/testLoggerPath" but only have: <ul><li>GET /</li><li>GET /:input</li><li>GET /error</li><li>GET /future</li><li>GET /halted</li><li>GET /unit</li><li>POST /create</li></ul>""")
      status shouldBe 404
    }
  }
}
