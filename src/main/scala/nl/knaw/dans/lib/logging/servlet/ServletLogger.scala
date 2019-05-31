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
import org.scalatra.{ ActionResult, ScalatraBase }

/**
 * This trait is the base for every servlet logger. It provides two abstract methods: `logRequest`
 * and `logResponse`.
 * Furthermore it adds a call to `logRequest` to the 'before filters' of `ScalatraBase`,
 * such that this method is called automatically on every request that comes in.
 * Also it extends the Scalatra's `renderResponse` method with a call to `logResponse`.
 */
trait AbstractServletLogger extends ScalatraBase {
  this: RequestLogFormatter with ResponseLogFormatter =>

  before() {
    logRequest()
  }

  override protected def renderResponse(actionResult: Any): Unit = {
    super.renderResponse(actionResult)

    logResponse {
      actionResult match {
        case ar: ActionResult => ar
        case _ => ActionResult(response.status, actionResult, Map.empty)
      }
    }
  }

  /**
   * Output the given request `logLine` as desired.
   *
   * @param logLine the log line to be outputted
   */
  protected def logRequest(logLine: String): Unit

  /**
   * Output the given response `logLine` as desired.
   *
   * @param logLine the log line to be outputted
   */
  protected def logResponse(logLine: String): Unit

  /**
   * Performs the side effect of the logging of the request.
   * This method is typically not called in user code, but rather in `ScalatraBase`'s `before` filter.
   */
  def logRequest(): Unit = logRequest(formatRequestLog)

  /**
   * Performs the side effect of the logging of the response, contained in the given `ActionResult`.
   *
   * @param actionResult the `ActionResult to be logged`
   */
  def logResponse(actionResult: ActionResult): Unit = logResponse(formatResponseLog(actionResult))
}

/**
 * Default servlet logger implemented with calls to `logger.info` for both the request and response logging.
 * Please note that the `logger` field is not implemented. It is most common to mixin `DebugEnhancedLogging`
 * for this, but other logger implementations can be provided as well.
 *
 * @example
 * {{{
 *   class ExampleServlet extends ScalatraServlet with ServletLogger with DebugEnhancedLogging
 * }}}
 */
trait ServletLogger extends AbstractServletLogger {
  this: ScalatraBase with RequestLogFormatter with ResponseLogFormatter =>

  protected val logger: Logger

  /**
   * @inheritdoc
   */
  override protected def logRequest(logLine: String): Unit = logger.info(logLine)

  /**
   * @inheritdoc
   */
  override protected def logResponse(logLine: String): Unit = logger.info(logLine)
}
