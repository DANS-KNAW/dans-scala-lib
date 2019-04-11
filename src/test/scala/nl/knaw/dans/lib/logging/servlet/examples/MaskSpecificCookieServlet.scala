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
package nl.knaw.dans.lib.logging.servlet.examples

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.logging.servlet._
import nl.knaw.dans.lib.logging.servlet.masked.Masker
import org.scalatra.{ Ok, ScalatraBase, ScalatraServlet }

trait NamedCookieMasker extends RequestLogExtensionBase with ResponseLogExtensionBase {
  this: ScalatraBase =>

  abstract override protected def formatHeader(header: HeaderMapEntry): HeaderMapEntry = {
    Masker.formatCookieHeader("cookie")(formatCookie)(super.formatHeader(header))
  }

  abstract override protected def formatResponseHeader(header: HeaderMapEntry): HeaderMapEntry = {
    Masker.formatCookieHeader("set-cookie")(formatCookie)(super.formatResponseHeader(header))
  }

  private def formatCookie(headerValue: String): String = {
    val cookieName = headerValue.replaceAll("=.*", "")
    if (cookieName == "my-cookie") {
      val cookieValue = headerValue.replaceFirst("[^=]+=", "")
      // replace sequences of chars without dots
      val maskedCookieValue = cookieValue.replaceAll("[^.]+", "****")
      s"$cookieName=$maskedCookieValue"
    }
    else headerValue
  }
}

class MaskSpecificCookieServlet extends ScalatraServlet
  with ServletLogger
  with PlainLogFormatter
  with NamedCookieMasker
  with DebugEnhancedLogging {

  get("/") {
    Ok("foobar")
  }
}
