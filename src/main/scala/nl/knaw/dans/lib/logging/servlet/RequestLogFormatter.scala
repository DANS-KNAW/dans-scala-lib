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
import org.scalatra.{ MultiParams, ScalatraBase }

import scala.collection.JavaConverters._

trait RequestLogFormatter {
  this: ScalatraBase =>

  /**
   * Constructs the `String` to be logged by `ServletLogger` about this request.
   * The resulted log line by default consists of:
   *   - the request's method (GET, POST, etc.)
   *   - the request's URL
   *   - the IP address this request is coming from
   *   - parameters sent with this request
   *   - headers sent with this request
   *
   * Please note that no data in this log line is masked. If masking is required, please refer
   * to the aggregated `MaskedLogFormatter` or the individual maskers in the `masked` package.
   *
   * @return the `String` to be logged
   */
  protected def formatRequestLog: String = {
    val method = request.getMethod
    val requestURL = request.getRequestURL.toString
    val formattedHeaders = formatHeaders(getHeaderMap(request)).makeString
    val formattedParams = formatParameters(multiParams).makeString
    val formattedRemoteAddress = formatRemoteAddress(Option(request.getRemoteAddr).getOrElse(""))

    // TODO perhaps more of https://github.com/scalatra/scalatra/blob/2.7.x/core/src/main/scala/org/scalatra/util/RequestLogging.scala#L70-L85
    s"request $method $requestURL remote=$formattedRemoteAddress; params=$formattedParams; headers=$formattedHeaders"
  }

  /**
   * Maps over all headers in this request and performs formatting (masking, prettyprinting, etc.)
   * for each of them. It returns a new `HeaderMap` with the same keys and the formatted values.
   *
   * By default it leaves the headers unformatted, but other implementations may provide other
   * formattings.
   *
   * Note that this does not change the formatting of the headers in the actual request.
   *
   * @param headers the headers to be formatted
   * @return a mapping of the headers' keys to their formatted values
   */
  protected def formatHeaders(headers: HeaderMap): HeaderMap = headers

  private def getHeaderMap(request: HttpServletRequest): HeaderMap = {
    // looks the same method as for ResponseLogFormatter, but everywhere different classes
    request.getHeaderNames.asScala.toSeq
      .map(name => name -> Option(request.getHeaders(name)).fold(Seq.empty[String])(_.asScala.toSeq))
      .toMap
  }

  /**
   * Maps over all parameters in this request and performs formatting (masking, prettyprinting, etc.)
   * for each of them. It returns a new `MultiParams` with the same keys and the formatted values.
   *
   * By default it leaves the parameters unformatted, but other implementations may provide other
   * formattings.
   *
   * Note that this does not change the content of the parameters in the actual request.
   *
   * @param params the parameters to be formatted
   * @return a mapping of the parameters' keys to their formatted values
   */
  protected def formatParameters(params: MultiParams): MultiParams = params

  /**
   * Formats (masking, prettyprinting, etc.) the request's remote address for logging purposes.
   * By default it leaves the address unformatted, but other implementations may provide other
   * formattings.
   *
   * Note that this does not change the remote address in the actual request.
   *
   * @param remoteAddress the remote address to be formatted
   * @return the formatted remote address
   */
  protected def formatRemoteAddress(remoteAddress: String): String = remoteAddress
}
