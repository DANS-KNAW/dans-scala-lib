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
import org.scalatra.{ ActionResult, ScalatraBase }

import scala.collection.JavaConverters._

trait ResponseLogFormatter {
  this: ScalatraBase =>

  /**
   * Constructs the `String` to be logged by `ServletLogger` about this response.
   *
   * @param actionResult the ActionResult to be logged
   * @param request      the original HTTP request object
   * @param response     the HTTP response object
   * @return the `String` to be logged
   */
  protected def formatResponseLog(actionResult: ActionResult)
                                 (implicit request: HttpServletRequest,
                                  response: HttpServletResponse): String = {
    val method = request.getMethod
    val status = actionResult.status
    val formattedAuthHeaders = formatResponseHeaders(getHeaderMap(response)).makeString
    val formattedActionHeaders = formatActionHeaders(actionResult.headers).makeString

    s"$method returned status=$status; authHeaders=$formattedAuthHeaders; actionHeaders=$formattedActionHeaders"
  }

  /**
   * Maps over all headers in this response and performs formatting (masking, prettyprinting, etc.)
   * for each of them. It returns a new `HeaderMap` with the same keys and the formatted values.
   *
   * Note that this does not change the formatting of the headers in the actual response.
   *
   * @param headers the headers to be formatted
   * @return a `Map` of formatted headers
   */
  protected def formatResponseHeaders(headers: HeaderMap): HeaderMap = headers.map(formatResponseHeader)

  /**
   * Formats (masking, prettyprinting, etc.) the given header's value for logging purposes.
   * By default it leaves the header as-is, but other implementations provide various other
   * possibilities.
   *
   * Note that this does not change the formatting of the specific header in the actual response.
   *
   * @param header the header to be formatted
   * @return the formatted header
   * @see MaskedSetCookie
   * @see MaskedRemoteUser
   */
  protected def formatResponseHeader(header: HeaderMapEntry): HeaderMapEntry = header

  private def getHeaderMap(response: HttpServletResponse): HeaderMap = {
    response.getHeaderNames.asScala.toSeq
      .map(name => name -> Option(response.getHeaders(name)).fold(Seq[String]())(_.asScala.toSeq))
      .toMap
  }

  /**
   * Formats (masking, prettyprinting, etc.) the headers from an `ActionResult`.
   *
   * Note that this method does not change the formatting of the headers in the actual response.
   *
   * @param actionHeaders the actionHeaders to be formatted
   * @return the formatted actionHeaders
   */
  protected def formatActionHeaders(actionHeaders: ActionHeadersMap): ActionHeadersMap = actionHeaders
}