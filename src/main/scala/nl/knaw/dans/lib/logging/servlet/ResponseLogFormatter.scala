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

import javax.servlet.http.HttpServletResponse
import org.scalatra.{ ActionResult, ScalatraBase }

import scala.collection.JavaConverters._

trait ResponseLogFormatter {
  this: ScalatraBase =>

  /**
   * Constructs the `String` to be logged by `ServletLogger` about this response.
   * The resulted log line by default consists of:
   *   - the request's method (GET, POST, etc.)
   *   - the status code of the `actionResult`
   *   - headers sent with this response, originating from the `HttpServletResponse` instance
   *   - headers sent with this response, originating from the `actionResult`
   *
   * Please note that no data in this log line is masked. If masking is required, please refer
   * to the aggregated `MaskedLogFormatter` or the individual maskers in the `masked` package.
   *
   * @param actionResult the ActionResult to be logged
   * @return the `String` to be logged
   */
  protected def formatResponseLog(actionResult: ActionResult): String = {
    val method = request.getMethod
    val requestURL = request.getRequestURL.toString
    val status = actionResult.status
    val formattedAuthHeaders = formatResponseHeaders(getHeaderMap(response))
    val formattedActionHeaders = formatActionHeaders(actionResult.headers).mapValues(List(_))
    val headers = (formattedAuthHeaders ++ formattedActionHeaders).makeString

    s"response $method $requestURL returned status=$status; headers=$headers"
  }

  /**
   * Maps over all headers in this response and performs formatting (masking, prettyprinting, etc.)
   * for each of them. It returns a new `HeaderMap` with the same keys and the formatted values.
   *
   * By default it leaves the headers unformatted, but other implementations may provide other
   * formattings.
   *
   * Note that this does not change the content of the headers in the actual response.
   *
   * @param headers the headers to be formatted
   * @return a mapping of the headers' keys to their formatted values
   */
  protected def formatResponseHeaders(headers: HeaderMap): HeaderMap = headers

  private def getHeaderMap(response: HttpServletResponse): HeaderMap = {
    response.getHeaderNames.asScala.toSeq
      .map(name => name -> Option(response.getHeaders(name)).fold(Seq.empty[String])(_.asScala.toSeq))
      .toMap
  }

  /**
   * Formats (masking, prettyprinting, etc.) the headers from an `ActionResult`.
   *
   * Note that this method does not change the content of the headers in the actual response.
   *
   * @param actionHeaders the actionHeaders to be formatted
   * @return the formatted actionHeaders
   */
  protected def formatActionHeaders(actionHeaders: ActionHeadersMap): ActionHeadersMap = actionHeaders
}
