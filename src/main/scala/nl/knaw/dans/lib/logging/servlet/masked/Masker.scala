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
package nl.knaw.dans.lib.logging.servlet.masked

import nl.knaw.dans.lib.logging.servlet.{ HeaderMapEntry, MultiParamsEntry }

object Masker {

  def formatCookie(value: String): String = {
    val cookieName = value.replaceAll("=.*", "")
    val cookieValue = value.replaceFirst("[^=]+=", "")
    // replace sequences of chars without dots
    val maskedCookieValue = cookieValue.replaceAll("[^.]+", "****")
    s"$cookieName=$maskedCookieValue"
  }

  /**
   * Formats the value of the request property RemoteAddr.
   * The default implementation masks the network and part of the host within that network.
   * Thus it is no longer identifying a person while we still might have a chance
   * to identify sessions in the log for debugging purposes.
   *
   * https://www.bluecatnetworks.com/blog/ip-addresses-considered-personally-identifiable-information/
   * in case of link rot paste the url at the tail of https://web.archive.org/web/20181030102418/
   *
   * Services without public access might not need to mask.
   */
  def formatRemoteAddress(remoteAddress: String): String = {
    remoteAddress match {
      case "0:0:0:0:0:0:0:1" | "127.0.0.1" | "::1" | // localhost
           "0:0:0:0:0:0:0:0" => remoteAddress // unspecified
      case _ if remoteAddress.contains(".") => remoteAddress
        .replaceAll("([.][0-9]+){3}$", ".**.**.**")
      case _ if remoteAddress.matches(":*[0-9A-F]+:+[0-9A-F]+") => remoteAddress
        .replaceAll("[0-9A-F]+$", "**")
      case _ => remoteAddress
        .replaceAll("(:[0-9A-F]+){3}$", ":**:**:**") // e.g: subnet ID + Interface ID of multicast
        .replaceAll("(:[0-9A-F]+){2}$", ":**:**")
        .replaceAll("(:[0-9A-F]+){1}$", ":**")
    }
  }

  def formatCookieHeader(headerName: String)(formatter: String => String): HeaderMapEntry => HeaderMapEntry = {
    formatTuple(_.toLowerCase == headerName)(formatter)
  }

  /**
   * Formats the value of headers with a case insensitive name ending with "authorization".
   * This implementation keeps the key like "basic", "digest" and "bearer" but masks the actual
   * credentials.
   */
  def formatAuthorizationHeader: HeaderMapEntry => HeaderMapEntry = {
    formatTuple(_.toLowerCase endsWith "authorization")(_.replaceAll(" .+", " *****"))
  }

  def formatRemoteUserHeader: HeaderMapEntry => HeaderMapEntry = {
    formatTuple(_.toLowerCase == "remote_user")(_ => "*****")
  }

  def formatAuthenticationParameter: MultiParamsEntry => MultiParamsEntry = {
    formatTuple(Seq("login", "password") contains _.toLowerCase)(_ => "*****")
  }

  def formatTuple(predicate: String => Boolean)
                 (format: String => String)
                 (tuple: (String, Seq[String])): HeaderMapEntry = {
    tuple match {
      case (name, values) if predicate(name) => name -> values.map(format)
      case otherwise => otherwise
    }
  }
}
